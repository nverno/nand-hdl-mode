// Simple program to get the docs from builtin chips
// @@FIXME: format for completion
#include <stdio.h>
#include <string.h>

enum STATE { IN, OUT };

int main(int argc, char *argv[]) {
  
  FILE* ofp = fopen("docs.txt", "w");
  fprintf(ofp, ";;; Generated from build/docs.exe -*- coding: utf-8 -*-\n(");
  
  while (--argc > 0) {
    FILE* f = fopen(*++argv, "r");
    fprintf(ofp, "(\"%.*s\" .", strlen(*argv)-4, *argv);
    int c, state = OUT;
  
    while ((c = getc(f)) != EOF) {
      if (c == '*') {
        if ((c = getc(f)) == '/') {
          fseek(ofp, -3, SEEK_CUR);
          putc('"', ofp);
          break;
        }
        if (c == '*') {
          putc(' ', ofp);
          putc('"', ofp);
          fseek(f, 5, SEEK_CUR);
          c = getc(f);
          state = IN;
        }
      }
      if (state == IN)
        putc(c, ofp);
    }
    putc(')', ofp);
    fclose(f);
  }
  
  putc(')', ofp);
  fclose(ofp);
  return 0;
}
