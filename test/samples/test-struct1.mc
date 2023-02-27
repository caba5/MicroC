struct Person {
    char name[3];
    int age;
    float netWorth;
};

void main(){
    struct Person p1, p2;
    p1.name[0] = 'A';
    p1.name[1] = 'B';
    p1.name[2] = 'C';
    p1.age = 23;
    p1.netWorth = 0.0;
    for (int i = 0; i < 3; ++i)
        cprint(p1.name[i]);
    cprint('\n');
    print(p1.age);
    fprint(p1.netWorth);
}
