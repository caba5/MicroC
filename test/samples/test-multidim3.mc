void foo(char arr[][3][3]){
    int i, j, k;
    for (i = 0; i < 3; ++i){
        for (j = 0; j < 3; ++j){
            for (k = 0; k < 3; ++k)
                cprint(arr[i][j][k]);
        }
    }
}

void main(){
    char arr[3][3][3];
    int i, j, k;
    for (i = 0; i < 3; ++i){
        for (j = 0; j < 3; ++j){
            for (k = 0; k < 3; ++k)
                arr[i][j][k] = 'a';
        }
    }
    foo(arr);
}
