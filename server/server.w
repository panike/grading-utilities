@*Introduction. This program is a server to assist in grading. The server puts
up a front end and on the back end outputs Lisp code that a lisp program
interprets.
@c
@h
@<Header inclusions@>@;
@<Global structure definitions@>@;
@<Global variable declarations@>@;
@<Global function declarations@>@;
@
@s rlimit int
@s section int
@s sockaddr_in int
@s stat int
@s rsrc int
@s insert_action int
@c
int main(int argc,char*argv[])
{
    char* config_file,*logfilename;
    int acceptfd, configfd, content_length, daemonize, ii, jj, listenfd;
    int numread, parser_state, port, readbuflen, workbuflen, kk;
    struct rlimit rlim;
    struct section sect;
    struct sockaddr_in sin;
    struct stat configstat;
    unsigned char *configbuf, *curr,*configbufend, *readbuf,*workbuf;
    struct rsrc hrsrc;
    struct insert_action actq;

    logfile=stderr;
    @<Initialize the program@>@;
    @<Parse the command line@>@;
    @<Read the config file@>@;
    @<Set up the sockets@>@;
    if(daemonize)
        @<Daemonize@>@;
    for(;;){
        fflush(logfile);
wait_for_connection:
        acceptfd=accept(listenfd,0,0);
        @<Get a request@>@;
        fprintf(logfile,"Possible DoS.\n");
        shutdown(acceptfd,SHUT_RDWR);
        close(acceptfd);
        goto wait_for_connection;
got_it_all:
#if 0
        for(ii=0;ii<hput.lncount;++ii)
            fprintf(logfile,"Line %d: %s\n",ii,hput.lines[ii]);
#endif
        @<Send a response@>@;
        shutdown(acceptfd,SHUT_RDWR);
        close(acceptfd);
    }
shut_it_down:
    @<Clean up after ourselves@>@;
    return 0;
}
@*2Outgoing HTTP buffer. We have code to contain HTTP responses.
@<Global struct...@>=
struct http_buf {
    char*header;
    char*body;
    int header_len;
    int body_len;
    int header_curr;
    int body_curr;
};
@ @<Global vari...@>=
struct http_buf outgoing;
@ @<Initialize...@>=
outgoing.header=(char*)malloc(1<<17);
outgoing.body=(char*)malloc(1<<17);
if(!outgoing.header || !outgoing.body){
    fprintf(logfile,"Could not allocate writing buffers.\n");
    _exit(0);
}
outgoing.header_len=outgoing.body_len=1<<17;
outgoing.header_curr=outgoing.body_curr=0;
@ @<Global fun...@>=
static void send_http_buf(int fd,struct http_buf*p);
static void write_http_header(struct http_buf*p,char*s);
static void write_http_body(struct http_buf*p,char*s);
@ @c
void send_http_buf(int fd,struct http_buf*p)
{
   int n;
   char buf[128];
   n=p->body_curr;
   if(p->body_curr>0) {
       if(n+p->header_curr>p->header_len)
           n-= p->body_curr+p->header_curr-p->header_len;
       snprintf(buf,80,"Content-Length: %d\r\n\r\n",n);
       write_http_header(p,buf);
       memcpy(&p->header[p->header_curr],p->body,n);
       p->header_curr+=n;
   }
   write(fd,p->header,p->header_curr);
#if 0
   fprintf(logfile,"\nSending the client:\n\n");
   fwrite(p->header,1,p->header_curr,logfile);
   fprintf(logfile,"\n\n");
#endif
}
@ @c
void write_http_header(struct http_buf*p,char*s)
{
    p->header_curr += snprintf(&p->header[p->header_curr],
        p->header_len-p->header_curr,"%s",s);
}
@ @c
void write_http_body(struct http_buf*p,char*s)
{
    p->body_curr += snprintf(&p->body[p->body_curr],
        p->body_len-p->body_curr,"%s",s);
}
@ @<Clean up...@>=
free(outgoing.header);
free(outgoing.body);
@*2Using URL resources. We maintain a queue to handle requests for
resources from clients.
@s hinput int
@<Global struct...@>=
@<Define a |struct hinput|@>@;
struct rsrc {
    char*name;
    void @[@] (*fn)(struct hinput*);
};
@ @<Global func...@>=
static struct rsrc*copy_rsrc(struct rsrc*p);
@ @c
struct rsrc*copy_rsrc(struct rsrc*p)
{
    struct rsrc*ret;
    ret=(struct rsrc*)malloc(sizeof(struct rsrc));
    if(!ret)
        return ret;
    ret->name=(char*)copy_string((unsigned char*)p->name);
    ret->fn=p->fn;
    return ret;
}
@ @<Global func...@>=
static void destroy_rsrc(struct rsrc*p)
{
    free(p->name);
}
@
@s queue_copyfn int
@s queue_destroyfn int
@s queue_fcns int
@<Global func...@>=
struct queue_fcns rsrc_queue_fcns = {
    @[.copy@]=(queue_copyfn)copy_rsrc,
    @[.destroy@]=(queue_destroyfn)destroy_rsrc
};
@ @<Global vari...@>=
Queue* resources;
@ @<Initialize the pro...@>=
resources=queue_allocate(&rsrc_queue_fcns);
@ @<Clean up...@>=
queue_destroy(resources);
@ @<Global funct...@>=
static void handle_request(struct rsrc*p,struct hinput*hp);
@ @c
void handle_request(struct rsrc*p,struct hinput* hp)
{
    if(prefix_matches(hp->rname,(unsigned char*)p->name))
        (*p->fn)(hp);
}
@ @<Define a |struct hinput|@>=
struct hinput {
    unsigned char*rname;
    unsigned char*rname_end;
    unsigned char *lines[128];
    int lncount;
    unsigned char* body;
    int known;
};
@ @<Global vari...@>=
struct hinput hput;
@ @<Send a response@>=
@<Get the resource name@>@;
if(strcmp((char*)hput.rname,"shutdown")==0){
    fprintf(logfile,"Saw a shutdown command.\n");
    outgoing.header_curr=0;
    outgoing.body_curr=0;
    write_http_header(&outgoing,"HTTP/1.1 200 OK\r\n");
    write_http_header(&outgoing,"Accept-Ranges: bytes\r\n");
    write_http_header(&outgoing,"Connection: close\r\n");
    write_http_header(&outgoing,"Content-Type: text/plain; charset=UTF-8\r\n");
    write_http_header(&outgoing,"Cache-Control: no-cache\r\n");
	write_http_body(&outgoing,"The server is shutting down.\r\n");
	send_http_buf(acceptfd,&outgoing);
    shutdown(acceptfd,SHUT_RDWR);
    close(acceptfd);
    goto shut_it_down;
}
@ @<Send a response@>=
if(strlen((char*)hput.rname)==0)
    do_start(&hput);
else {
    hput.known=0;
    queue_iterate(resources,(queue_iterator)check_name,&hput);
    if(hput.known != 0)
        queue_iterate(resources,(queue_iterator)handle_request,&hput);
    else
        send_unknown(&outgoing);
}
send_http_buf(acceptfd,&outgoing);
@ @<Global func...@>=
void check_name(struct rsrc*p,struct hinput*hp);
@ @c
void check_name(struct rsrc*p,struct hinput*hp)
{
    if(prefix_matches(hp->rname,(unsigned char*)p->name))
        hp->known = 1;
}
@ @<Initialize the ...@>=
hrsrc.name="start";
hrsrc.fn=do_start;
queue_insert(resources,&hrsrc);
@ @<Initialize the ...@>=
hrsrc.name="insert";
hrsrc.fn=do_insert;
queue_insert(resources,&hrsrc);
@ @<Initialize the ...@>=
hrsrc.name="section";
hrsrc.fn=do_section;
queue_insert(resources,&hrsrc);
@ @<Global func...@>=
static void do_insert(struct hinput*);
@ @c
void do_insert(struct hinput*hp)
{
    char* toks[2];
    char* resource_toks[3];
    struct sect_info sinfo;
    int ii,gt,comm;
#if 0
    fprintf(logfile,"rname is \"%s\".\n",hp->rname);
#endif
    start_html(&outgoing);
    if(split((unsigned char*)hp->rname,'/',resource_toks,3)!=2 ||
            strcmp(resource_toks[0],"insert")!=0)
        @<Unsuccessful attempt@>@;
    else {
        @<Split |hp->body| into pairs of names and values@>@;
        my_quicksort(submission_names,submission_values,numpairs);
        @<Decide what to do based on the |gradetype| value@>@;
    }
    finish_html(&outgoing);
}
@ @<Global func...@>=
static void start_html(struct http_buf* htb);
static void finish_html(struct http_buf* htb);
static void send_unknown(struct http_buf* htb);
@ @c
void start_html(struct http_buf* htb)
{
    htb->header_curr=0;
    htb->body_curr=0;
    write_http_header(htb,"HTTP/1.1 200 OK\r\n");
    write_http_header(htb,"Accept-Ranges: bytes\r\n");
    write_http_header(htb,"Connection: close\r\n");
    write_http_header(htb,"Content-Type: text/html; charset=UTF-8\r\n");
    write_http_header(htb,"Cache-Control: no-cache\r\n");
    write_http_body(htb,"<html>\n<head><title>A small grading server</title>");
    write_http_body(htb,"</head>\n<body>\n");
}
@ @c
static void send_unknown(struct http_buf* htb)
{
    htb->header_curr=0;
    htb->body_curr=0;
    write_http_header(htb,"HTTP/1.1 404 Not Found\r\n");
    write_http_header(htb,"Accept-Ranges: bytes\r\n");
    write_http_header(htb,"Connection: close\r\n");
    write_http_header(htb,"Content-Type: text/plain; charset=UTF-8\r\n");
    write_http_header(htb,"Cache-Control: no-cache\r\n");
    write_http_body(htb,"I do not know where to find that. Sorry\n");
}
@ @c
void finish_html(struct http_buf* htb)
{
    write_http_body(htb,"</body>\n</html>\n");
}
@*1Handling insertion actions.  This is where grades actually get entered into
the database.  This is supposed to be flexible. One can add functions to
the queue |action_queue| and the server should know what to do.
@<Decide what to do...@>=
gt=find_name((unsigned char*)"gradetype",numpairs);
if(gt>=0){
    sinfo.rname=resource_toks[1];
    sinfo.gt=(char*)submission_values[gt];
    sinfo.comm="";
    comm=find_name((unsigned char*)"comment",numpairs);
    if(comm>=0)
        sinfo.comm=(char*)submission_values[comm];
#if 0
    fprintf(logfile,"Looking to do action \"%s\".\n",sinfo.rname);
#endif
    queue_iterate(action_queue,(queue_iterator)handle_action,&sinfo);
    @<Successful attempt@>@;
}@+else
    @<Unsuccessful attempt@>@;
@ @<Global struct...@>=
@<Define a |struct sect_info|@>@;
struct insert_action {
    char *name;
    void @[@] (*fn)(struct sect_info*);
};
@ @<Global funct...@>=
static struct insert_action* copy_action(struct insert_action*pia);
@ @c
struct insert_action* copy_action(struct insert_action*pia)
{
    struct insert_action*ret;
    ret=(struct insert_action*)malloc(sizeof(struct insert_action));
    if(!ret)
        return ret;
    ret->name=(char*)copy_string((unsigned char*)pia->name);
    ret->fn=pia->fn;
    return ret;
}
@ @<Global func...@>=
static void destroy_action(struct insert_action*pia)
{
    free(pia->name);
}
@ @<Global func...@>=
struct queue_fcns action_queue_fcns = {
    @[.copy@]=(queue_copyfn)copy_action,
    @[.destroy@]=(queue_destroyfn)destroy_action
};
@ @<Global vari...@>=
Queue* action_queue;
@ @<Initialize the...@>=
action_queue=queue_allocate(&action_queue_fcns);
@ @<Clean up...@>=
queue_destroy(action_queue);
@ @<Global func...@>=
static void handle_action(struct insert_action*p,struct sect_info*s)
{
#if 0
    fprintf(logfile,"Here in handle_action.\n");
    fprintf(logfile,"p->name is \"%s\", s->gt is \"%s\".\n",
            p->name,s->gt);
#endif
    if(strcmp(p->name,s->gt)==0)
        (*p->fn)(s);
}
@ @<Global func...@>=
static void handle_att(struct sect_info*);
static void handle_supp_att(struct sect_info*);
static void handle_part(struct sect_info*);
static void handle_real_work(struct sect_info*);
@ @<Initialize the...@>=
actq.name="att";
actq.fn=handle_att;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="supp-att";
actq.fn=handle_supp_att;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="part";
actq.fn=handle_part;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="exam";
actq.fn=handle_real_work;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="quiz";
actq.fn=handle_real_work;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="hw";
actq.fn=handle_real_work;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="lec-quiz";
actq.fn=handle_real_work;
queue_insert(action_queue,&actq);
@ @<Initialize the...@>=
actq.name="group-work";
actq.fn=handle_real_work;
queue_insert(action_queue,&actq);
@*2Handling assignments, quizzes, and exams. @c
void handle_real_work(struct sect_info*sinfo)
{
    int pss;
    sinfo->poss="";
    pss=find_name((unsigned char*)"poss",numpairs);
    if(pss>=0){
        sinfo->poss=(char*)submission_values[pss];
        if(strlen(sinfo->poss)!=0)
            queue_iterate(section_queue,(queue_iterator)do_real_work,sinfo);
    }
}
@ @<Global func...@>=
static void do_real_work(struct section*p,struct sect_info*sfo);
@ @c
void do_real_work(struct section*p,struct sect_info*sfo)
{
    if(strcmp((char*)p->name,sfo->rname)==0){
        fprintf(p->fp,";; Adding info about %s %s\n",sfo->gt,sfo->comm);
        sfo->fp=p->fp;
        sfo->idx=0;
        queue_iterate(p->pupils,(queue_iterator)p_real_work,sfo);
        fflush(p->fp);
        if(allfp){
            sfo->fp=allfp;
            sfo->rname="all";
            sfo->idx=0;
            fprintf(allfp,";; Adding info about %s %s\n",sfo->gt,sfo->comm);
            queue_iterate(p->pupils,(queue_iterator)p_real_work,sfo);
            fflush(allfp);
        }
    }
}
@ @<Global func...@>=
static void p_real_work(char* name,struct sect_info*sfo);
@ @c
void p_real_work(char* name,struct sect_info*sfo)
{
    char buf[64];
    int ern;
    sprintf(buf,"earned%d",sfo->idx);
    ern=find_name((unsigned char*)buf,numpairs);
    if(ern>=0){
        if(strlen((char*)submission_values[ern])==0)
            submission_values[ern]=(unsigned char*)"0";
        fprintf(sfo->fp,"(add-%s (get-%s-person \"%s\") %s %s)\n",
                sfo->gt,sfo->rname, name, submission_values[ern],sfo->poss);
    }
    ++sfo->idx;
}
@*2Handling participation.
@c
void handle_part(struct sect_info*sinfo)
{
    queue_iterate(section_queue,(queue_iterator)do_part,sinfo);
}
@ @<Global func...@>=
static void do_part(struct section*p,struct sect_info*sfo);
@ @c
void do_part(struct section*p,struct sect_info*sfo)
{
    if(strcmp((char*)p->name,sfo->rname)==0){
        sfo->fp=p->fp;
        fprintf(p->fp,";; Adding participation data %s.\n",sfo->comm);
        sfo->idx=0;
        queue_iterate(p->pupils,(queue_iterator)p_part,sfo);
        fflush(p->fp);
        if(allfp){
            sfo->idx=0;
            sfo->rname="all";
            sfo->fp=allfp;
            fprintf(allfp,";; Adding participation data %s.\n",sfo->comm);
            queue_iterate(p->pupils,(queue_iterator)p_part,sfo);
            fflush(allfp);
        }
    }
}
@ @<Global func...@>=
static void p_part(char* name,struct sect_info*sfo);
@ @c
void p_part(char* name,struct sect_info*sfo)
{
    char buf[64];
    int ern;
    sprintf(buf,"attended%d",sfo->idx);
    ern=find_name((unsigned char*)buf,numpairs);
    if(ern>=0 && strcmp((char*)submission_values[ern],"0")==0)
        fprintf(sfo->fp,"(add-class-participation "
                "(get-%s-person \"%s\"))\n", sfo->rname, name);
    ++sfo->idx;
}
@*2Handling attendance. @c
void handle_att(struct sect_info*sinfo)
{
    queue_iterate(section_queue,(queue_iterator)do_att,sinfo);
}
@ @<Global func...@>=
static void do_att(struct section*p,struct sect_info*sfo);
@ @c
void do_att(struct section*p,struct sect_info*sfo)
{
    if(strcmp((char*)p->name,sfo->rname)==0){
        sfo->fp=p->fp;
        fprintf(p->fp,";; Adding attendance data %s.\n",sfo->comm);
        sfo->idx=0;
        queue_iterate(p->pupils,(queue_iterator)p_att,sfo);
        fflush(p->fp);
        if(allfp){
            sfo->idx=0;
            sfo->rname="all";
            sfo->fp=allfp;
            fprintf(allfp,";; Adding attendance data %s.\n",sfo->comm);
            queue_iterate(p->pupils,(queue_iterator)p_att,sfo);
            fflush(allfp);
        }
    }
}
@ @<Global func...@>=
static void p_att(char* name,struct sect_info*sfo);
@ @c
void p_att(char* name,struct sect_info*sfo)
{
    char buf[64];
    int ern;
    sprintf(buf,"attended%d",sfo->idx);
    ern=find_name((unsigned char*)buf,numpairs);
    if(ern>=0){
        if(strlen((char*)submission_values[ern])==0)
            submission_values[ern]=(unsigned char*)"0";
        fprintf(sfo->fp,"(add-att (get-%s-person \"%s\") %s 1)\n",
                sfo->rname, name, submission_values[ern]);
    }
    ++sfo->idx;
}
@*2Handling supplemental attendance. @c
void handle_supp_att(struct sect_info*sinfo)
{
    queue_iterate(section_queue,(queue_iterator)do_supp_att,sinfo);
}
@ @<Global func...@>=
static void do_supp_att(struct section*p,struct sect_info*sfo);
@ @c
void do_supp_att(struct section*p,struct sect_info*sfo)
{
    if(strcmp((char*)p->name,sfo->rname)==0){
        sfo->fp=p->fp;
        fprintf(p->fp,";; Adding supp. "
		"attendance data %s.\n",sfo->comm);
        sfo->idx=0;
        queue_iterate(p->pupils,(queue_iterator)p_supp_att,sfo);
        fflush(p->fp);
        if(allfp){
            sfo->idx=0;
            sfo->rname="all";
            sfo->fp=allfp;
            fprintf(allfp,";; Adding supp. attendance data %s.\n",sfo->comm);
            queue_iterate(p->pupils,(queue_iterator)p_supp_att,sfo);
            fflush(allfp);
        }
    }
}
@ @<Global func...@>=
static void p_supp_att(char* name,struct sect_info*sfo);
@ @c
void p_supp_att(char* name,struct sect_info*sfo)
{
    char buf[64];
    int ern;
    sprintf(buf,"attended%d",sfo->idx);
    ern=find_name((unsigned char*)buf,numpairs);
    if(ern>=0){
        if(strlen((char*)submission_values[ern])==0)
            submission_values[ern]=(unsigned char*)"0";
        fprintf(sfo->fp,"(add-supp-att (get-%s-person \"%s\") %s 1)\n",
                sfo->rname, name, submission_values[ern]);
    }
    ++sfo->idx;
}
@ @<Unsuccessful attempt@>={
    write_http_body(&outgoing,"You were not successful.<br>\n");
    queue_iterate(section_queue,(queue_iterator)output_section_info,0);
    write_http_body(&outgoing,"<a href=\"/shutdown\">Shutdown "
        "the server</a><br>\n");
}
@ @<Successful attempt@>=
write_http_body(&outgoing,"You were successful.<br>\n");
queue_iterate(section_queue,(queue_iterator)output_section_info,&sinfo);
write_http_body(&outgoing,"<a href=\"/shutdown\">Shutdown "
        "the server</a><br>\n");
@*1Utility functions. A couple functions to do sort and search.
@<Global func...@>=
static int find_name(unsigned char*p,int num);
static void my_quicksort(unsigned char**nm,unsigned char**va,int num);
@*2 Binary search. This is binary search.
@c
int find_name(unsigned char*p,int num)
{
    int mid,left,right,cmp;
    mid=num/2;
    left=0;
    right=num-1;
    while(left < right){
        cmp=strcmp((char*)p,(char*)submission_names[mid]);
        if(cmp>0){
            left=mid+1;
            mid = (left+right)/2;
        }@+else if(cmp<0){
            right=mid-1;
            mid=(left+right)/2;
        }@+else if(cmp==0)
            return mid;
    }
    if(strcmp((char*)p,(char*)submission_names[mid])==0)
        return mid;
    return -1;
}
@*2Quicksort. This is quicksort. @c
void my_quicksort(unsigned char**nm,unsigned char**va,int num)
{
    unsigned char*p;
    int ii,jj;
    ii=0;

    if(num<=1)
        return;
    for(jj=1;jj<num;++jj){
        if(strcmp((char*)nm[jj],(char*)nm[ii])<0){
            p=nm[jj];
            nm[jj]=nm[ii+1];
            nm[ii+1]=nm[ii];
            nm[ii]=p;
            p=va[jj];
            va[jj]=va[ii+1];
            va[ii+1]=va[ii];
            va[ii]=p;
            ++ii;
        }
    }
    my_quicksort(nm,va,ii);
    ++ii;
    my_quicksort(&nm[ii],&va[ii],num-ii);
}
@ @<Global vari...@>=
unsigned char* submission_names[128];
unsigned char* submission_values[128];
int numpairs;
@ @<Split |hp->bo...@>=
numpairs=split(hp->body,'&',(char**)submission_names,128);
for(ii=0;ii<numpairs;++ii){
    toks[0]=toks[1]="";
    split(submission_names[ii],'=',toks,2);
    submission_values[ii]=(unsigned char*)toks[1];
    remove_encoding(submission_names[ii]);
    remove_encoding(submission_values[ii]);
}
@*2Removing encoding from URLs.
@<Global func...@>=
static void remove_encoding(unsigned char*s);
@ @c
void remove_encoding(unsigned char*s)
{
    unsigned char*p;
    p=s;
    while(*p){
        if(*p == '+'){
            *s = ' ';
            ++p;
        }@+else if(*p == '%'){
            *s = from_hex(p+1);
            p+=3;
        }@+else{
            *s=*p;
            ++p;
        }
        ++s;
    }
    if(*s)
        *s='\0';
}
@ @<Global func...@>=
static unsigned char from_hex(unsigned char*p);
@ @c
unsigned char from_hex(unsigned char*p)
{
    unsigned char ret;
    ret = 0x0;
    @<Convert a hex character to a number@>@;
    ret <<= 4;
    ++p;
    @<Convert a hex character to a number@>@;
    return ret;
}
@ @<Convert a hex character to a number@>=
switch(*p){
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        ret += *p - '0';
        break;
    case 'A': case 'B': case 'C':
    case 'D': case 'E': case 'F':
        ret += (*p - 'A') + 0xa ;
        break;
    case 'a': case 'b': case 'c':
    case 'd': case 'e': case 'f':
        ret += (*p - 'a') + 0xa ;
    default: break;
}
@ @<Global vari...@>=
unsigned char token[256];
@ @<Global func...@>=
static void do_start(struct hinput*hp);
static void do_section(struct hinput*hp);
@ @c
void do_start(struct hinput*hp)
{
    struct sect_info sinfo;
    start_html(&outgoing);
    queue_iterate(section_queue,(queue_iterator)output_section_info,&sinfo);
    write_http_body(&outgoing,"<a href=\"/shutdown\">Shutdown "
            "the server</a><br>\n");
    finish_html(&outgoing);
}
@ @<Global func...@>=
static void output_section_info(struct section*p,void*sfo);
@ @c
void output_section_info(struct section*p,void*sfo)
{
    char buf[80];
    sprintf(buf,"<a href=\"/section/%s\">Section %s</a><br>\n",
            p->name,p->name);
    write_http_body(&outgoing,buf);
}
@ @<Define a |struct sect_info|@>=
struct sect_info {
    char* rname;
    char* gt;
    int idx;
    FILE* fp;
    char*poss;
    char*comm;
};
@ @<Global func...@>=
static int split(unsigned char*s,char c,char**toks,int numtoks);
@ @c
int split(unsigned char*s,char c,char**toks,int numtoks)
{
    int ii;
    for(ii=0;ii<numtoks;){
        while(*s && *s == c){
            *s='\0';
            ++s;
        }
        if(*s == '\0')
            break;
        toks[ii]=(char*)s;
        ++ii;
        while(*s && *s != c)
            ++s;
        if(*s == '\0')
            break;
    }
    return ii;
}
@ @c
void do_section(struct hinput*hput)
{
    struct sect_info sfo;
    char* toks[3];
    char buf[128];
    start_html(&outgoing);
    if(split(hput->rname,'/',toks,3)==2 && strcmp(toks[0],"section")==0){
        sfo.rname=toks[1];
        sfo.idx=0;
        sprintf(buf,"<form action=\"/insert/%s\" method=\"p"
                "ost\" enctype=\"application/x-www-form-urlencoded\">\n",
                toks[1]);
        write_http_body(&outgoing,buf);
        @<Produce the menu for insertion@>@;
        write_http_body(&outgoing,"<table border=\"0\">\n");
        queue_iterate(section_queue,(queue_iterator)produce_insert_table,
                &sfo);
    @<End a table, produce submit button@>@;
        write_http_body(&outgoing,"<table border=\"0\">\n");
        sfo.idx=0;
        queue_iterate(section_queue,(queue_iterator)produce_att_insert_table,
                &sfo);
    @<End a table, produce submit button@>@;
        write_http_body(&outgoing,"</form>\n");
    }
    finish_html(&outgoing);
}
@
@<End a table, produce submit button@>=
write_http_body(&outgoing,"</table><br>\n");
write_http_body(&outgoing,"<input type=\"submit\">\n");
write_http_body(&outgoing,"<input type=\"reset\">\n");
@ @<Global func...@>=
static void produce_insert_table(struct section*p,struct sect_info*s);
static void student_insert(char*name,struct sect_info*s);
@ @c
void student_insert(char*name,struct sect_info*s)
{
    char buf[128];
    write_http_body(&outgoing,"<tr>\n");
    snprintf(buf,127,"<td>%d. %s",s->idx+1,name);
    buf[127]='\0';
    write_http_body(&outgoing,buf);
    write_http_body(&outgoing,"</td>\n");
    snprintf(buf,127,"<td>Earned: <input type=\"text\""
            " size=\"4\" name=\"earned%d\"></td>\n",s->idx);
    write_http_body(&outgoing,buf);
    write_http_body(&outgoing,"</tr>\n");
    ++s->idx;
}
@ @c
void produce_insert_table(struct section*p,struct sect_info*s)
{
    if(strcmp(s->rname,(char*)p->name)==0)
        queue_iterate(p->pupils,(queue_iterator)student_insert,s);
}
@ @<Global func...@>=
static void produce_att_insert_table(struct section*p,struct sect_info*s);
static void student_att_insert(char*name,struct sect_info*s);
@ @c
void student_att_insert(char*name,struct sect_info*s)
{
    char buf[128];
    write_http_body(&outgoing,"<tr>\n");
    snprintf(buf,127,"<td>%d. %s",s->idx+1,name);
    buf[127]='\0';
    write_http_body(&outgoing,buf);
    write_http_body(&outgoing,"</td>\n");
    snprintf(buf,127,"<td><input type=\"radio\""
            " name=\"attended%d\" value=\"1\" checked=\"yes\"></td>\n",s->idx);
    write_http_body(&outgoing,buf);
    snprintf(buf,127,"<td><input type=\"radio\""
            " name=\"attended%d\" value=\"0\"></td>\n",
            s->idx);
    write_http_body(&outgoing,buf);
    write_http_body(&outgoing,"</tr>\n");
    ++s->idx;
}
@ @c
void produce_att_insert_table(struct section*p,struct sect_info*s)
{
    if(strcmp(s->rname,(char*)p->name)==0)
        queue_iterate(p->pupils,(queue_iterator)student_att_insert,s);
}
@ @<Produce the menu for insertion@>=
write_http_body(&outgoing,"Type: <select name=\"gradetype\">\n");
write_http_body(&outgoing,"<option value=\"hw\">Homework</option>\n");
write_http_body(&outgoing,"<option value=\"quiz\">Quiz</option>\n");
write_http_body(&outgoing,"<option value=\"exam\">Exam</option>\n");
write_http_body(&outgoing,"<option value=\"lec-quiz\">Lecture Quiz"
    "</option>\n");
write_http_body(&outgoing,"<option value=\"group-work\">Group Work</option>\n");
write_http_body(&outgoing,"<option value=\"att\">Attendance</option>\n");
#if 0
write_http_body(&outgoing,"<option value=\"part\">Participation</option>\n");
write_http_body(&outgoing,"<option value=\"supp-att\">Supp. "
	"Attendance</option>\n");
#endif
write_http_body(&outgoing,"</select>\n");
write_http_body(&outgoing,"Possible: <input name=\"poss\" type=\"text\" "
    "size=\"4\">\n");
write_http_body(&outgoing,"Comment: <input name=\"comment\" type=\"text\" "
    "size=\"4\">\n");
@ @<Get the resource...@>=
hput.rname=hput.lines[0];
while(*hput.rname && *hput.rname != ' ')
    ++hput.rname;
while(*hput.rname && *hput.rname == ' ')
    ++hput.rname;
hput.rname_end=hput.rname;
while(*hput.rname_end && *hput.rname_end != ' ')
    ++hput.rname_end;
*hput.rname_end='\0';
++hput.rname; /* Clear out the initial \.{/} */
@ @<Get a request@>=
for(ii=0;ii<readbuflen;){
    numread=read(acceptfd,&readbuf[ii],readbuflen-ii);
    if(numread <= 0){
        shutdown(acceptfd,SHUT_RDWR);
        close(acceptfd);
        goto wait_for_connection;
    }
#if 0
    fprintf(logfile,"OK, finished reading.\n");
#endif
    ii+=numread;
    memcpy(workbuf,readbuf,ii);
    for(jj=ii;jj<readbuflen;++jj)
        workbuf[jj]='\0';
    workbuflen=ii;
    break_into_lines(workbuf,workbuflen,hput.lines,&hput.lncount);
    for(jj=0;jj<hput.lncount;++jj){
        if(strlen((char*)hput.lines[jj])==0)
            @<Check for \.{POST} and body@>@;
    }
}
@ At this point, we have the header.  If there is a \.{POST} command
we will have a body.  So we check to see if we have all the body.
@<Check for \.{P...@>={
    if(prefix_matches((unsigned char*)hput.lines[0],
                (unsigned char*)"POST")){
        @<Grab the \.{Content-Length} header value@>@;
        @<Check that the body has the same length ...@>@;
    }@+else if(prefix_matches((unsigned char*)hput.lines[0],
                (unsigned char*)"GET"))
        goto got_it_all;
}
@ @<Grab the \.{Content-Length} header value@>=
content_length=0;
for(kk=0;kk<jj;++kk)
    if(prefix_matches(hput.lines[kk],(unsigned char*)"Content-Length")){
        content_length=get_int(hput.lines[kk]);
#if 0
        fprintf(logfile,"Content-Length is %d.\n",content_length);
#endif
        break;
    }
@ @<Check that the body has the same length as the \.{Content-Length}@>=
hput.body=(unsigned char*)0;
if(hput.lines[jj+1] && strlen((char*)hput.lines[jj+1])==content_length){
    hput.body=hput.lines[jj+1];
#if 0
    fprintf(logfile,"The body is \"%s\".\n",hput.body);
#endif
    goto got_it_all;
}
@ @<Initialize the program...@>=
readbuflen=(1<<17);
readbuf=(unsigned char*)malloc(readbuflen);
workbuf=(unsigned char*)malloc(readbuflen);
if(!readbuf || !workbuf){
    fprintf(logfile,"Could not allocate memory for reading buffers.\n");
    _exit(0);
}
@ @<Clean up...@>=
free(readbuf);
free(workbuf);
@ @<Global func...@>=
static int get_int(unsigned char* s)
{
    while(*s && *s != ':') ++s;
    if(*s == ':')
        return atoi((char*)(s+2));
    return 0;
}
@ @<Global funct...@>=
static void break_into_lines(unsigned char*wb,int blen,unsigned char**lns,int*lnct);
@ @c
void break_into_lines(unsigned char*wb,int blen,unsigned char**lns,int*lnct)
{
    unsigned char* wbend,*p;
    lns[0]=wb;
    *lnct=0;
    wbend=wb+blen;
    for(p=wb;p<wbend;++p)
        if(*p == '\n'){
            *p='\0';
            if(p>wb && *(p-1)=='\r')
                *(p-1)='\0';
            ++*lnct;
            lns[*lnct]=p+1;
        }
}
@ @<Global funct...@>=
static int prefix_matches(const unsigned char*s,const unsigned char*pre);
@ @c
int prefix_matches(const unsigned char*s,const unsigned char*pre)
{
    const unsigned char*t;
    for(t=pre;*t;++t){
        if(*t != *s)
           return 0;
       ++s;
    }
    return 1;
}
@ @<Parse the command line@>=
for(ii=1;ii<argc;++ii){
    if(strcmp("-f",argv[ii])==0){
        ++ii;
        config_file=argv[ii];
    }
    if(strcmp("-h",argv[ii])==0)
        @<Print out a helpful message@>@;
    if(strcmp("-d",argv[ii])==0)
        daemonize=1;
    if(strcmp("-p",argv[ii])==0){
        ++ii;
        port=atoi(argv[ii]);
    }
    if(strcmp("-l",argv[ii])==0){
        ++ii;
       logfilename=argv[ii];
    }
}
@ @<Initialize the program@>=
port=3025; /* Default port */
daemonize=0; /* Default is not a daemon */
config_file=(char*)0; /* Error not to specify a config file */
@ @<Parse...@>=
if(config_file==(char*)0)
    @<Print out...@>@;
@ @<Print out a help...@>={
    fprintf(logfile,"Usage: %s <-f config file> [-p port] [-h] [-d]"
            " <-l logfilename>\n",argv[0]);
    fprintf(logfile,"Default port is 3025.\n");
    fprintf(logfile,"-h for this message.\n");
    fprintf(logfile,"-d to daemonize.\n");
    fprintf(logfile,"-l for the logfile.\n");
    _exit(0);
}
@ @<Header inclusions@>=
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "queue.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
@
@s Queue int
@<Global structure definitions@>=
struct section {
    unsigned char* name;
    Queue* pupils;
    FILE* fp;
};
@ @<Global variable declarations@>=
Queue* section_queue;
@ @<Global function declarations@>=
static struct section* copy_section(struct section*p);
static void destroy_section(struct section*p);
struct queue_fcns section_queue_fcns = {
    @[.copy@] = (queue_copyfn)&copy_section,
    @[.destroy@] = (queue_destroyfn)&destroy_section
};
@ @<Initialize...@>=
section_queue=queue_allocate(&section_queue_fcns);
@ @<Clean up...@>=
queue_destroy(section_queue);
@ @c
struct section* copy_section(struct section*p)
{
    struct section*ret;
    if(p == (struct section*)0)
       return p;
    ret=(struct section*)malloc(sizeof(struct section));
    if(ret != (struct section*)0){
        ret->name=p->name;
        ret->pupils=p->pupils;
        ret->fp=p->fp;
    }
    return ret;
}
@ @c
void destroy_section(struct section*p)
{
    free(p->name);
    queue_destroy(p->pupils);
    fclose(p->fp);
    free(p);
}
@ @<Read the config file@>=
configfd=open(config_file,O_RDONLY);
if(configfd<0){
    fprintf(logfile,"Could not open file \"%s\".\n",config_file);
    _exit(0);
}
@ @<Read the config file@>=
fstat(configfd,&configstat);
configbuf=(unsigned char*)malloc(configstat.st_size);
if(configbuf == (unsigned char*)0){
    fprintf(logfile,"Could not allocate memory for the config file.\n");
    _exit(0);
}
@ @<Clean up...@>=
free(configbuf);
@ @<Read the confi...@>=
for(ii=0;ii<configstat.st_size;ii+=numread){
    numread=read(configfd,&configbuf[ii],configstat.st_size-ii);
    if(numread < 0) {
        fprintf(logfile,"Error reading from the config file.\n");
        _exit(0);
    }
}
close(configfd);
@ We have certain keywords, \.{Section}, \.{Student}, and \.{File}, \.{All},
and \.{End}.
@<Read the conf...@>=
curr=configbuf;
configbufend=configbuf+configstat.st_size;
parser_state=0;
@ @<Read the conf...@>=
while(curr<configbufend){
    get_token(&curr,configbufend,token);
#if 0
    fprintf(logfile,"Got token \"%s\".\n",token);
#endif
    switch(parser_state) {
        case 0: @<Handle parser state 0@>@+@[break@];@;
        case 1: @<Handle parser state 1@>@+@[break@];@;
        case 2: @<Handle parser state 2@>@+@[break@];@;
    case 3: @<Handle parser state 3@>@+@[break@];@;
        case 4: @<Handle parser state 4@>@+@[break@];@;
        case 5: @<Handle parser state 5@>@+@[break@];@;
        default: fprintf(logfile,"Parser error.\n");
                _exit(0);
                break;
    }
}
@ @<Read the conf...@>=
fprintf(logfile,"We reached the end of file without seeing \"End\".\n");
fprintf(logfile,"That is a fatal error my friend.\n");
_exit(0);
done_parsing:
@ @<Read the conf...@>=
#if 0
fprintf(logfile,"Now check...\n\n");
queue_iterate(section_queue,(queue_iterator)print_list,0);
#endif
@ @<Global func...@>=
#if 0
static void print_list(struct section*p);
static void print_student_list(unsigned char*p);
#endif
@ @c
#if 0
void print_list(struct section*p)
{
    fprintf(logfile,"name: %s\n",p->name);
    queue_iterate(p->pupils,(queue_iterator)print_student_list,0);
}
#endif
@ @c
#if 0
void print_student_list(unsigned char*p)
{
    fprintf(logfile,"student name: %s\n",p);
}
#endif
@ @<Handle parser state 0@>=
if(strcmp((char*)token,"Section")==0)
    parser_state=5;
else{
    fprintf(logfile,"Syntax error.  Expected to see Section first.\n");
    _exit(0);
}
@ We are introducing a new section.  So we file away the old section.
@<Handle parser state 1@>=
if(strcmp((char*)token,"Section")==0){
    parser_state=2;
    queue_insert(section_queue,&sect);
    sect.pupils=queue_allocate(&pupils_queue_fcns);
    sect.fp=(FILE*)0;
    sect.name=(unsigned char*)0;
}
@ @<Header incl...@>=
#include <string.h>
@ @<Global funct...@>=
static unsigned char* copy_string(unsigned char*p)
{
    unsigned char* ret;
    ret=(unsigned char*)malloc(strlen((char*)p)+1);
    strcpy((char*)ret,(char*)p);
    return ret;
}
struct queue_fcns pupils_queue_fcns = {
    @[.copy@]= (queue_copyfn)&copy_string,
    @[.destroy@]=&free
};
@ @<Handle parser state 1@>=
else if(strcmp((char*)token,"Student")==0){
    get_rest_of_line(&curr,configbufend,token);
    queue_insert(sect.pupils,token);
}
@ @<Header...@>=
#include <ctype.h>
@ @<Global func...@>=
static void get_rest_of_line(unsigned char**curr,unsigned char*end,unsigned char*tok);
@ @c
void get_rest_of_line(unsigned char**curr,unsigned char*end,unsigned char*tok)
{
    *tok='\0';
    while(*curr < end && isspace(**curr)){
        if(**curr == '\n'){
            ++lineno;
            return;
        }
        ++*curr;
    }
    while(*curr < end && **curr != '\n'){
        *tok=**curr;
        ++tok;
        *tok='\0';
        ++*curr;
    }
    if(**curr == '\n')
        ++lineno;
}
@ @<Global vari...@>=
int lineno;
@ @<Initialize...@>=
lineno=1;
@ @<Handle parser state 1@>=
else@+if(strcmp((char*)token,"File")==0)
    parser_state=4;
else if(strcmp((char*)token,"All")==0)
    parser_state=3;
@ @<Handle parser state 1@>=
else if(strcmp((char*)token,"End")==0){
    queue_insert(section_queue,&sect);
    goto done_parsing;
}@+else{
    fprintf(logfile,"Unknown token \"%s\" in state 1.\n",token);
    _exit(0);
}
@ @<Handle parser state 2@>=
sect.name=copy_string(token);
sect.pupils=queue_allocate(&pupils_queue_fcns);
parser_state=1;
@ @<Handle parser state 3@>=
allfp=fopen((char*)token,"a");
if(!allfp){
    fprintf(logfile,"Could not open the all archive \"%s\".\n",token);
    _exit(0);
}
parser_state=1;
@ @<Global vari...@>=
FILE* allfp;
@ @<Initialize the...@>=
allfp=(FILE*)0;
@ @<Clean up...@>=
if(allfp)
    fclose(allfp);
@ @<Handle parser state 4@>=
sect.fp=fopen((char*)token,"a");
if(!sect.fp){
   fprintf(logfile,"Could not open file \"%s\".\n",token);
   _exit(0);
}
parser_state=1;
@ @<Handle parser state 5@>=
sect.name=copy_string(token);
sect.pupils=queue_allocate(&pupils_queue_fcns);
parser_state=1;
@ @<Global func...@>=
static void get_token(unsigned char**,unsigned char*,unsigned char*);
@ @c
void get_token(unsigned char**curr,unsigned char* end,unsigned char*tok)
{
   while(*curr < end && isspace(**curr)) {
       if(**curr == '\n')
           ++lineno;
      ++*curr;
   }
    while(*curr < end && !isspace(**curr)){
        *tok=**curr;
        ++tok;
        *tok='\0';
       ++*curr;
    }
}
@ @<Header incl...@>=
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
@ @<Set up the sockets@>=
listenfd=socket(PF_INET,SOCK_STREAM,0);
if(listenfd<0){
    fprintf(logfile,"Could not get a socket.\n");
    _exit(0);
}
ii=1;
setsockopt(listenfd,SOL_SOCKET,SO_REUSEADDR,&ii,sizeof(ii));
@ @<Clean up...@>=
close(listenfd);
@ @<Set up the sockets@>=
sin.sin_family=AF_INET;
sin.sin_port=htons(port);
sin.sin_addr.s_addr=INADDR_ANY;
if(bind(listenfd,(struct sockaddr*)&sin,sizeof(sin))) {
    fprintf(logfile,"Error number is %d.\n",errno);
    fprintf(logfile,"Error binding the socket.\n");
    _exit(0);
}
@ @<Header incl...@>=
#include <errno.h>
@ @<Set up the so...@>=
if(listen(listenfd,3)){
    fprintf(logfile,"Error listening on the socket.\n");
    _exit(0);
}
@ @<Global vari...@>=
FILE*logfile;
@ @<Daemonize@>={
    setsid();
    ioctl(0, TIOCNOTTY); /* Kill the terminal */
    getrlimit(RLIMIT_NOFILE,&rlim);
    for(ii=0;ii<rlim.rlim_cur;++ii)
        if(((ii != listenfd) && !match_filenos(ii,section_queue))
        || (allfp && (fileno(allfp) !=ii)))
            close(ii);
    if(fork())
        _exit(0);
    if((logfile=fopen(logfilename,"a"))==(FILE*)0)
        _exit(0);
    fprintf(logfile,"OK, I am here writing to the logfile.\n");
}
@ @<Clean up...@>=
if(logfile && logfile != stderr)
    fclose(logfile);
@ @<Header incl...@>=
#include <termios.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
@ @<Global func...@>=
static int match_filenos(int ii,Queue* sq);
@ @c
int match_filenos(int ii,Queue* sq)
{
    struct fno fnop;
    fnop.fd=ii;
    fnop.result=0;
    queue_iterate(sq,(queue_iterator)&check_fileno,&fnop);
    return fnop.result;
}
@ @<Global struct...@>=
struct fno {
    int fd;
    int result;
};
@ @<Global func...@>=
static void check_fileno(struct section*sp,struct fno* fnop);
@ @c
void check_fileno(struct section*sp,struct fno* fnop)
{
    if(fileno(sp->fp)==fnop->fd)
        fnop->result=1;
}
