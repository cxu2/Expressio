//
//  DFA.c skeleton code
//  
//
//  Created by David Han on 2/11/18.
//

#include "DFA.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>


struct dfa_state{
  int ID;
    
  
}

  struct dfa_t{
    /*
Q is a finite set of states.

∑ is a finite set of symbols called the alphabet.

δ is the transition function where δ: Q × ∑ → Q

q0 is the initial state from where any input is processed (q0 ∈ Q).

F is a set of final state/states of Q (F ⊆ Q).
    */
    dfa_state *states;//points to collection of states
    char *alphabet;
    //transition function, maybe a dictionary
    dfa_state init;
    dfa_state *final;
    
  }

  //DFA functions?
    dfa_t * init(dfa_state * states, char * alphabet, dfa_state init, dfa_state *final){
    
    }

dfa_state evaluate(char *input, struct dfa_t*  ){//takes list of characters and returns a result state

  for(int i = 0;i < strlen(input); i++){
    //do transition for each char
    ;
  }
    
}


bool accept(char *input, struct dfa_t*){
  dfa_state * check = evaluate(input);
  return check.compare(dfa_t->final);    
}
