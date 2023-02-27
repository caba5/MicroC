int global = 100;

int fun(int a){
    return a + global;
}

int main(){
    int var = 5;
    print(fun(var));
    return 0;
}
