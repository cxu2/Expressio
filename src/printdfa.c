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
int printdfa(struct dfa_t * d){
  //hacking for insight

  printf("\nnstates:  %i", d->nstates);
  printf("\nalphabet: ");
  for(int i = 0; i < d->nsym; i++){
    printf("%c ", d->alphabet[i]);
  }
  printf("\nnsym:     %i", d->nsym);
  printf("\nstart:    %i", d->init);
  printf("\nfin:      ");
  for(int i = 0; i < d->nfin; i++){
    printf("%i ", d->final[i]);
  }
  printf("\nnfin:     %i", d->nfin);
  printf("\ndelta:    %p\n", d->delta);

  /*  int * ptrhack = (int *) d;
  for(int i = 0; i < 20; i++){
    printf("int:   %i            %p\n", *ptrhack, ptrhack);
    ptrhack++;
    }*/

  return 0;
}
