/*
 * File: builtin.c
 * Date: 2018-03-26
 *
 * PLT Spring 2018
 * Expressio Project
 * 
 * Created by Chengtian Xu <cx2168@columbia.edu>
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct {
  char operator;
  char character;
  char * left;
  char * right;
} tree_t;


int printb(bool res) {
  if (res == true) {
    printf("true\n");
  } else {
    printf("false\n");
  }
  return 0;
}

int printr_helper(tree_t* regex_ptr) {
  if (regex_ptr -> operator == 'n') {
    if (regex_ptr -> character == '#') {
      printf("{.}");
    } else {
      printf("{{.}}");
    }
  } else if (regex_ptr -> operator == 'l') {
    printf("%c", regex_ptr -> character);
  } else if (regex_ptr -> operator == '*') {
    printr_helper((tree_t *)(regex_ptr -> left));
    printf("%c ", regex_ptr -> operator); 
  } else if (regex_ptr -> operator == '\'') {
    printf(" %c", regex_ptr -> operator);
    printr_helper((tree_t *)(regex_ptr -> left));
  } else {
    printf("( ");
    printr_helper((tree_t *)(regex_ptr -> left));
    printf(" %c ", regex_ptr -> operator);  
    printr_helper((tree_t *)(regex_ptr -> right));
    printf(" )");
  }
 
	return 0;
}


int printr(tree_t* regex_ptr) {
  printr_helper(regex_ptr);
  printf("\n");
  return 0;
}

void free_regex(tree_t* t) {
  if (t == NULL) {
    // printf("*********free_null**********\n");
    return;
  }
  if (t -> operator == 'n' || t -> operator == 'l') {
    // printf("*********free_nary_lit**********\n");
    free(t);
    t = NULL;
  } else if (t -> operator == '*' || t -> operator == '\'') {
    // printf("*********free_star_comp**********\n");
    free_regex((tree_t*)(t -> left));
    free(t);
    t = NULL;
  } else {
    // printf("*********free_binary**********\n");
    free_regex((tree_t*)(t -> left));
    free_regex((tree_t*)(t -> right));
    free(t);
    t = NULL;
  }
}


// tree_t* lefttok(tree_t* regex_ptr) {
//   if (regex_ptr -> operator == 'l') {
//     printf("lit does not have a left token\n");
//     exit(1);
//   }
//   printf("top operator is %c\n", regex_ptr -> operator);
//   return (tree_t *)(regex_ptr -> left);
// }

// tree_t* righttok(tree_t* regex_ptr) {
//   if (regex_ptr -> operator == 'r') {
//     printf("lit does not have a right token\n");
//     exit(1);
//   } else if (regex_ptr -> operator == '*') {
//     printf("Kleene star does not have a right token");
//     exit(1);
//   }
//   return (tree_t *)(regex_ptr -> right);
// }

char litchar(tree_t* regex_ptr) {
  if (regex_ptr -> operator != 'l') {
    printf("cannot get char from non-lit regexp, operator has type %d\n", regex_ptr -> operator);
    exit(1);
  } else {
    printf("char is: %c\n", regex_ptr -> character);
    return regex_ptr -> character;
  }
}


bool identical(tree_t* r1, tree_t* r2) {
  if (r1 == NULL && r2 == NULL)
    return true;

  if (r1 != NULL && r2 != NULL) {
    return (r1 -> operator == r2 -> operator &&
            r1 -> character == r2 -> character &&
            identical((tree_t*)(r1 -> left), (tree_t*)(r2 -> left)) &&
            identical((tree_t*)(r1 -> right), (tree_t*)(r2 -> right)));
  }
  return false;
}


tree_t* Zero(){
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = 'n';
  t -> character = '#';
  t -> left = NULL;
  t -> right = NULL;
  return t;
}

tree_t* One() {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = 'n';
  t -> character = '@';
  t -> left = NULL;
  t -> right = NULL;
  return t;
}

tree_t* Mult(tree_t* left, tree_t* right) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '^';
  t -> character = '\0';
  t -> left = &left -> operator;
  t -> right = &right -> operator;
  return t;
}

tree_t* Plus(tree_t* left, tree_t* right) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '|';
  t -> character = '\0';
  t -> left = &left -> operator;
  t -> right = &right -> operator;
  return t;
}

tree_t* Star(tree_t* regex) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '*';
  t -> character = '\0';
  t -> left = &regex -> operator;
  t -> right = NULL;
  return t;
}

tree_t* Comp(tree_t* regex) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '\'';
  t -> character = '\0';
  t -> left = &regex -> operator;
  t -> right = NULL;
  return t;
}

tree_t* And(tree_t* left, tree_t* right) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '&';
  t -> character = '\0';
  t -> left = &left -> operator;
  t -> right = &right -> operator;
  return t;
}

tree_t* clone(tree_t* regex_ptr) {
  if (regex_ptr -> operator == 'n') {
    return regex_ptr -> character == '#' ? Zero() : One();
  } 

  tree_t* t;
  if (regex_ptr -> operator == 'l') {
    t = (tree_t*)malloc(sizeof(tree_t));
    if (t == NULL) {
      printf("malloc failed due to out of memory.");
      exit(1);
    }
    t -> operator = 'l';
    t -> character = regex_ptr -> character;
    t -> left = NULL;
    t -> right = NULL;
  } else if (regex_ptr -> operator == '*' || regex_ptr -> operator == '\'') {
    t = (tree_t*)malloc(sizeof(tree_t));
    if (t == NULL) {
      printf("malloc failed due to out of memory.");
      exit(1);
    }
    t -> operator = regex_ptr -> operator;
    t -> character = '\0';
    tree_t* leftTree = clone((tree_t*)(regex_ptr -> left));
    t -> left = &leftTree -> operator;
    t -> right = NULL;
  } else {
    t = (tree_t*)malloc(sizeof(tree_t));
    if (t == NULL) {
      printf("malloc failed due to out of memory.");
      exit(1);
    }
    t -> operator = regex_ptr -> operator;
    t -> character = '\0';
    tree_t* leftTree = clone((tree_t*)(regex_ptr -> left));
    t -> left = &leftTree -> operator;
    tree_t* rightTree = clone((tree_t*)(regex_ptr -> right));
    t -> right = &rightTree -> operator;
  }
  return t;
}

int getOpOrder(tree_t* r) {
  switch(r -> operator) {
    case 'n':
      return r -> character == '#' ? 0 : 1;
    case 'l':
      return 2;
    case '|':
      return 3;
    case '^':
      return 4;
    case '*':
      return 5;
    case '&':
      return 6;
    case '\'':
      return 7;
    default:
      return 8;
  }
}

// returns -1 for smaller, 0 for equal and 1 for bigger
int biggerThan(tree_t* r1, tree_t* r2) {
  // printf("biggerThan\n");
  int r1Order = getOpOrder(r1);
  int r2Order = getOpOrder(r2);
  if (r1Order < r2Order) {
    return -1;
  } else if (r1Order == r2Order) {
    // printf("same order!\n");
    if (r1 -> operator == 'n' && r2 -> operator == 'n') {
      return 0;
    } else if (r1 -> operator == 'l' && r2 -> operator == 'l') {
      if (r1 -> character == r2 -> character) {
        return 0;
      } else {
        return r1 -> character > r2 -> character;
      }
    } else if (r1 -> operator == '*' || r1 -> operator == '\'') {
      return biggerThan((tree_t*)(r1 -> left), (tree_t*)(r2 -> left));
    } else {
      // printf("else\n");
      if (! biggerThan((tree_t*)(r1 -> left), (tree_t*)(r2 -> left))) {
        return biggerThan((tree_t*)(r1 -> right), (tree_t*)(r2 -> right));
      }
      return 0;
    }
  } else {
    return 1;
  }
}

tree_t* star(tree_t* r) {
  if (r -> operator == 'n') {
    return One();
  } else if (r -> operator == '*') {
    return star((tree_t*)(r -> left));
  } else {
    return Star(r);
  }
}

tree_t* mult(tree_t* r1, tree_t* r2) {
  if (r2 -> operator == 'n' && r2 -> character == '#'){
    return r2;
  } else if (r1 -> operator == 'n' && r1 -> character == '#') {
    return r1;
  } else if (r2 -> operator == 'n' && r2 -> character == '@') {
    return r1;
  } else if (r1 -> operator == 'n' && r1 -> character == '@') {
    return r2;
  } else if (r1 -> operator == '^') {
    return Mult((tree_t*)(r1 -> left), mult((tree_t*)(r1 -> right), r2));
  } else if (biggerThan(r1, r2) && r1 -> operator == '*' && r2 -> operator == '*') {
    return star((tree_t*)(r1 -> left));
  } else {
    return Mult(r1, r2);
  }
}

tree_t* plus(tree_t* r1, tree_t* r2) {
  if (r2 -> operator == 'n' && r2 -> character == '#'){
    return r1;
  } else if (r1 -> operator == 'n' && r1 -> character == '#') {
    return r2;
  } else if (r1 -> operator == '|') {
    return plus((tree_t*)(r1 -> left), plus((tree_t*)(r1 -> right), r2));
  } else if (r2 -> operator == '|') {
    if (biggerThan(r1, (tree_t*)(r2 -> left)) == 0) {
      return Plus((tree_t*)(r2 -> left), (tree_t*)(r2 -> right));
    } else if (biggerThan(r1, (tree_t*)(r2 -> left)) == -1) {
      return Plus(r1, Plus((tree_t*)(r2 -> left), (tree_t*)(r2 -> right)));
    } else {
      return Plus((tree_t*)(r2 -> left), plus(r1, (tree_t*)(r2 -> right)));
    }
  } else {
    if (biggerThan(r1, r2) == 0) {
      return r1;
    } else if (biggerThan(r1, r2) == -1) {
      return Plus(r1, r2);
    } else {
      return Plus(r2, r1);
    }
  }
}


tree_t* comp(tree_t* r) {
  if (r -> operator == '\'') {
    return (tree_t*)(r -> left);
  }

  return Comp(r);
}


tree_t* intersect(tree_t* r1, tree_t* r2) {
  if (r1 -> operator == 'n' && r1 -> character == '#'){
    return Zero();
  } else if (r1 -> operator == '\'' && identical((tree_t*)(r1 -> left), Zero())) {
    return r2;
  } else {
    return And(r1, r2);
  }
}


int nullable(tree_t* r) {
  if (r -> operator == 'n' && r -> character == '#') {
    return 0;
  } else if (r -> operator == 'n' && r -> character == '@') {
    return 1;
  } else if (r -> operator == 'l') {
    return 0;
  } else if (r -> operator == '|') {
    return nullable((tree_t*)(r -> left)) || nullable((tree_t*)(r -> right));
  } else if (r -> operator == '^') {
    return nullable((tree_t*)(r -> left)) && nullable((tree_t*)(r -> right));
  } else if (r -> operator == '*') {
    return 1;
  } else if (r -> operator == '&') {
    return nullable((tree_t*)(r -> left)) && nullable((tree_t*)(r -> right));
  } else if (r -> operator == '\'') {
    return !nullable((tree_t*)(r -> left));
  } else {
    return 0;
  }
}

tree_t* constant(tree_t* r) {
  return nullable(r) ? One() : Zero();
}

tree_t* derivative(tree_t* regex_ptr, char c) {
  switch(regex_ptr -> operator) {
    // Zero or One
    case 'n':
      // printf("case 'n'\n");
      return Zero();
    // Lit
    case 'l':
      // printf("case 'l': regex_ptr-> character = %c, c = %c\n", regex_ptr -> character, c);
      return regex_ptr -> character == c ? One() : Zero();
    // Star
    case '*':
      // printf("case '*'\n");
      return mult(derivative((tree_t*)(regex_ptr -> left), c), star((tree_t*)(regex_ptr -> left)));
    // Plus
    case '|':
      // printf("case '|'\n");
      return plus(derivative((tree_t*)(regex_ptr -> left), c), derivative((tree_t*)(regex_ptr -> right), c));
    // Mult
    case '^':
      // printf("case '^'\n");
      return plus(mult(derivative((tree_t*)(regex_ptr -> left), c), (tree_t*)regex_ptr -> right), 
        mult(constant((tree_t*)(regex_ptr -> left)), derivative((tree_t*)(regex_ptr -> right), c)));
    case '\'':
      // printf("case '\''\n");
      return comp(derivative((tree_t*)(regex_ptr -> left), c));
    case '&':
      // printf("case '&'\n");
      return intersect(derivative((tree_t*)(regex_ptr -> left), c), derivative((tree_t*)(regex_ptr -> right), c));
    default:
      return regex_ptr -> character == '#' ? Zero() : One();
  }
}

int matchesHelper(tree_t* regex_ptr, char* str) {
  // printr(regex_ptr);
  if (regex_ptr -> operator == 'n' && regex_ptr -> character == '#') {
    // printf("Zero does not match with anything\n");
    free(regex_ptr);
    return 0;
  }
  // printf("regex_ptr->operator == %c, char = %c\n", regex_ptr -> operator, str[0]);

  // empty string matches only with epsilon
  if (str[0] == '\0') {
    // printf("the end of the input string\n");
    tree_t* one = One();
    tree_t* constt = constant(regex_ptr);
    int res = biggerThan(constt, one);
    free(regex_ptr);
    free(constt);
    free(one);
    
    return res == 0 ? 1 : 0;
  }

  char curChar = str[0];
  // printf("current character is %c\n", curChar);
  return matchesHelper(derivative(regex_ptr, curChar), ++str);
}

bool matches(tree_t* regex_ptr, char* str) {
  // empty language does not match with any string
  // printf("matches is called, matching string %s\n", str);
  tree_t* cloned = clone(regex_ptr);
  bool same = identical(regex_ptr, cloned);
  return matchesHelper(cloned, str) == 0 ? false : true;
}
