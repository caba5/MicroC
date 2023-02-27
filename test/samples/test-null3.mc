void main(){
    int x = 100;
    int *p1 = &x;
    int *p2 = &x;
    if (p1 == p2)
        cprint('T');
    else
        cprint('F');
    p2 = NULL;
    if (p1 == p2)
        cprint('T');
    else
        cprint('F');
    if (p2 == NULL)
        cprint('T');
    else
        cprint('F');
}
