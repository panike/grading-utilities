#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char lines[512][80];

void print_header(int c)
{
	int ii;
	printf("\\halign{");
	for(ii=1;ii<c;++ii)
		printf("\\bf#\\ &\\sl#\\ &#\\hskip30pt&");
	printf("\\bf#\\ &\\sl#\\ &#\\cr\n");
}

void print_entry(char*s)
{
	if(!*s)
		return;
	while(*s){
		if(*s == '\t')
			printf("&");
		else if(*s == ' ')
			printf("&");
		else if(*s == '\n')
			; /* Do nothing */
		else
			printf("%c",*s);
		++s;
	}
}

int main(int argc,char**argv)
{
	int numlines;
	int addlines;
	int cline;
	int tablelines;
	int pline;
	int ii;
	int columns;

	columns=3;
	for(ii=1;ii<argc;++ii){
		if(strcmp(argv[ii],"-cols")==0){
			++ii;
			columns = atoi(argv[ii]);
		}
	}
	numlines=0;
	while(numlines < 512 && fgets(&lines[numlines][0],80,stdin))
		++numlines;
	addlines=((numlines%columns)==0)?0:(columns-(numlines%columns));
	for(cline=0;cline<addlines;++cline){
		lines[numlines][0]='\0';
		++numlines;
	}
	print_header(columns);
	tablelines = numlines/columns;
	for(cline=0; cline<tablelines;++cline){
		pline=cline;
		for(ii=1;ii<columns;++ii){
			print_entry(&lines[pline][0]);
			printf("&\n");
			pline+=tablelines;
		}
		print_entry(&lines[pline][0]);
		printf("\\cr\n");
	}
	printf("}\n");
	return 0;
}
