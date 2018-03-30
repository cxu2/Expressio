#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

struct dfa_t{
  /*
    Q is a finite set of states.
    ∑ is a finite set of symbols called the alphabet.
    δ is the transition function where δ: Q × ∑ → Q
    q0 is the initial state from where any input is processed (q0 ∈ Q).
    F is a set of final state/states of Q (F ⊆ Q).
  */
  int nstates;
  char *alphabet;
  int nsym;
  int init;//by state id
  int *final;//by state id
  int nfin;
  int ** delta;
};


//DFA functions
int printdfa(struct dfa_t d){
  //hacking for insight
  int * finbytes = (int *) &d.final;
  int * alphabytes = (int *) &d.alphabet;
  int * deltabytes = (int *) &d.delta;

  printf("\nnsts: %i", d.nstates);
  printf("\naplha: %i", *alphabytes);
  printf("\nnsym: %i", d.nsym);
  printf("\n start: %i", d.init);
  printf("\n fin: %i", *finbytes);
  printf("\nfin: %i", d.nfin);
  printf("\ndelta: %i\n", *deltabytes);

  int * ptrhack = &d.nsym;
  int * firstint = (int *) ptrhack;
  printf("\nfirst int: %i", *firstint);

  return 0;
}
