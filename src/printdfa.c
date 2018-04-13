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
  int * delta;
};


//DFA functions
int printdfa(struct dfa_t * d){

  // print states
  printf("\nnstates:  %i", d->nstates);

  // alphabet 
  printf("\nalphabet: ");
  for(int i = 0; i < d->nsym; i++){
    printf("%c ", d->alphabet[i]);
  }
  printf("\nnsym:     %i", d->nsym);

  // start state
  printf("\nstart:    %i", d->init);

  // accept states
  printf("\nfin:      ");
  for(int i = 0; i < d->nfin; i++){
    printf("%i ", d->final[i]);
  }
  printf("\nnfin:     %i", d->nfin);

  // delta function displayed in funciton table
  printf("\ndelta:    \n");
  printf("        ");
  for(int i = 0; i < d->nsym; i++){
    printf("|%4c ", d->alphabet[i]);
  }
  printf("\n    ----");
  for(int i = 0; i < d->nsym; i++){
    printf("|-----");
  }
  for(int i = 0; i < d->nstates; i++){
     printf("\n    %4i|", i);
    for(int j = 0; j < d->nsym; j++){
      printf("%4i  ", d->delta[i*d->nsym+j]);
    }
  }

  printf("\n");

  return 0;
}
