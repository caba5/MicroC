void foo(char str[], int size){
    int i;
    for (i = 0; i < size; ++i)
        cprint(str[i]);
}

void main(){
    char s[] = "this is a string";
    foo(s, 16);
}
