int foo(int x){
    return print(x), x+10;
}

void main(){
    int res = foo(990);
    print(res);
}
