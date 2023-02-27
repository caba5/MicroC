void foo(int arr[][3]){
    int i, j;
    for (i = 0; i < 3; ++i){
        for (j = 0; j < 3; ++j){
            print(arr[i][j]);
        }
    }
}

void main(){
    int arr[3][3];
    int i, j;
    for (i = 0; i < 3; ++i){
        for (j = 0; j < 3; ++j){
            arr[i][j] = j + 1 + 3 * i;
        }
    }
    foo(arr);
}
