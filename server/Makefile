include ../rules.mk

SOURCES:=Makefile queue.w server.w

ifndef V
	QUIET_AR=@echo "AR $@";
	QUIET_AR_OUTPUT=>/dev/null 2>&1
endif
libqueue.a: queue_allocate.o queue_destroy.o queue_destroy_data.o \
	queue_insert.o queue_iterate.o queue_len.o
	$(QUIET_AR)$(AR) -rv $@ $? $(QUIET_AR_OUTPUT)

queue_allocate.c queue_destroy.c queue_destroy_data.c: queue.c
queue_insert.c queue_iterate.c queue_len.c: queue.c

queue.h: queue.c

server.c: queue.h

server: server.o libqueue.a
	$(QUIET_CC)$(CC) -o $@ server.o -L. -lqueue

clean:
	/bin/rm -f $(filter-out $(SOURCES),$(shell /bin/ls))
