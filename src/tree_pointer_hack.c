#include <stdio.h>

struct tree_t {
  char ex;
  char * left;
  char * right;
};

void traverse(char * imbedded){
  char ** left = (char**)(imbedded+8);
  char ** right = left+1;
  if(*left){
    traverse(*left);
  }
  printf("%c",*imbedded);
  if(*right){
    traverse(*right);
  }
}

void traverse_with_struct_ptr(char * imbedded){
  struct tree_t * node = (struct tree_t *)imbedded;
  if(node->left){
    traverse_with_struct_ptr(node->left);
  }
  printf("%c", node->ex);
  if(node->right){
    traverse_with_struct_ptr(node->right);
  }
}

int main(){
  struct tree_t root;
  struct tree_t L;
  struct tree_t R;

  root.ex = '^';
  L.ex = '0';
  R.ex = '1';
  
  root.left = &L.ex;
  root.right = &R.ex;
  L.left = NULL;
  L.right = NULL;
  R.left = NULL;
  R.right = NULL;

  printf("Addr root char: %p\n", &root.ex);
  printf("Addr root left ptr: %p\n", &root.left);
  printf("Addr root right ptr: %p\n", &root.right);
  
  printf("Addr root char +8: %p\n", &root.ex+8);
  printf("Addr root left ptr +1: %p\n", &root.left+1);

  traverse(&root.ex);
  printf("\n");
  traverse_with_struct_ptr(&root.ex);
  return 0;
}
