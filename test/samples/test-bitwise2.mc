void main(){
    int bit1 = 5;
    int bit2 = 2;
    print(bit1 | bit2);
    print(bit1 & bit2);
    print(bit1 ^ bit2);
    print(2 << 2);
    print(8 >> 2);
    print(~bit2);
    print(bit1 | bit2 ^ (bit1 << 2) >> 8);
}
