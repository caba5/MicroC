void foo(char str[]){
    str[0] = 'a';
}

void main(){
    char str[] = "cbc";
    foo(str);
    cprint(str[0]);
}
