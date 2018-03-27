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
  printf("\nnsym: %i", d.nsym);
  //  printf("\n 0: %c", d.alphabet[0]);
  printf("\nnsts: %i", d.nstates);
  printf("\nfin: %i", d.nfin);
  //printf("\n 0: %c", d.final[0]);

  return 0;
}
