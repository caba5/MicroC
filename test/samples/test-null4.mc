int *p = NULL;

void printIsNull(){
    if (p == NULL)
        cprint('T');
    else
        cprint('F');
}

void main(){
    printIsNull();
    int x = 10;
    p = &x;
    printIsNull();
}
