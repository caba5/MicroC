void main(){
    print(sizeof(1));
    print(sizeof(1.0));
    print(sizeof('a'));
    print(sizeof(true));
    int *ptr;
    print(sizeof(ptr));
    int x = 10;
    ptr = &x;
    print(sizeof(*ptr));
}
