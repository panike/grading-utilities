@ We write a library to handle queues.
@(queue.h@>=
#ifndef _PANIKE_QUEUE_H
#define _PANIKE_QUEUE_H
@h
@<Structure definitions@>;
@<Function declarations@>@;
#endif
@ @<Structure def...@>=
struct queue_node {
    struct queue_node*next; /* next node in the queue */
    void*p; /* The data */
};
@ @<Struc...@>=
typedef void* @[@] (*queue_copyfn)(void *p);
typedef void @[@] (*queue_destroyfn)(void*p);
struct queue_fcns {
    queue_copyfn copy;
    queue_destroyfn destroy;
};
@ @<Structure def...@>=
typedef struct {
    struct queue_node*head;
    struct queue_node*tail;
    struct queue_fcns*fns;
} Queue;
@ The idea is that $p$ is the data, and $r$ is the result.
@<Structure def...@>=
typedef void @[@] (*queue_iterator)(void*p,void*r);
@ @<Funct...@>=
Queue* queue_allocate(struct queue_fcns*fns);
@ @(queue_allocate.c@>=
#include "queue.h"
#include <stdlib.h>
Queue* queue_allocate(struct queue_fcns*fns)
{
    Queue*ret;
    if((ret=(Queue*)malloc(sizeof(Queue)))==(Queue*)0)
        return ret;
    ret->fns=fns;
    ret->head=ret->tail=(struct queue_node*)0;
    return ret;
}
@ @<Funct...@>=
void* queue_insert(Queue*q,void*p);
@ @(queue_insert.c@>=
#include "queue.h"
#include <stdlib.h>
void* queue_insert(Queue*q,void*p)
{
    struct queue_node*nnde;

    nnde=(struct queue_node*)malloc(sizeof(struct queue_node));
    if(nnde == ((struct queue_node*)0))
        return ((void*)0);
    nnde->next=(struct queue_node*)0;
    nnde->p=(*q->fns->copy)(p);
    if(!nnde->p){
        free(nnde);
        return ((void*)0);
    }
    if(!q->head && !q->tail)
        q->head=q->tail=nnde;
    else {
        q->tail->next=nnde;
        q->tail=nnde;
    }
    return nnde->p;
}
@ @<Funct...@>=
void queue_destroy_data(Queue*q);
@ @(queue_destroy_data.c@>=
#include "queue.h"
#include <stdlib.h>
void queue_destroy_data(Queue*q)
{
    struct queue_node*nde,*next;

    for(nde=q->head;nde;nde=next){
        next=nde->next;
        (*q->fns->destroy)(nde->p);
        free(nde);
    }
    q->head=q->tail=(struct queue_node*)0;
};
@ @<Funct...@>=
void queue_destroy(Queue*q);
@ @(queue_destroy.c@>=
#include "queue.h"
#include <stdlib.h>
void queue_destroy(Queue*q)
{
    queue_destroy_data(q);
    free(q);
}
@ @<Funct...@>=
void queue_iterate(Queue*q,queue_iterator fn,void*r);
@ @(queue_iterate.c@>=
#include "queue.h"
void queue_iterate(Queue*q,queue_iterator fn,void*r)
{
    struct queue_node*nde;

    for(nde=q->head;nde;nde=nde->next)
        (*fn)(nde->p,r);
}
@ Here is an example how to use an iterator
@<Functi...@>=
unsigned int queue_len(Queue*q);
@ @(queue_len.c@>=
#include "queue.h"
static void inc(void*p,unsigned int*r)
{
    if(r)
        ++*r;
}
@ @(queue_len.c@>=
unsigned int queue_len(Queue*q)
{
    unsigned int r=0;
    queue_iterate(q,(queue_iterator)&inc,&r);
    return r;
}
