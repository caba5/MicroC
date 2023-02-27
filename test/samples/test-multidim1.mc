int main(){
    int a[3][3];
    int i;
    int j;
    for (i = 0; i < 3; ++i){
        for (j = 0; j < 3; ++j){
            a[i][j] = (i + 1) * (j + 1);
            print(a[i][j]);
        }
    }
    return 0;
}
