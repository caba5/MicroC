void printAreEq(int* p1, int* p2){
    if (p1 == p2)
        cprint('T');
    else
        cprint('F');
}

void main(){
    int *x = NULL;
    int *y = NULL;
    printAreEq(x, y);
    int val = 32;
    x = &val;
    printAreEq(x, y);
    y = x;
    printAreEq(x, y);
}
