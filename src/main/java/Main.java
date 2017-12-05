//public class Main {
//    public Main() {
//    }
//
//    public static void main(String[] var0) {
//        Foo var1 = new Foo();
//        var1.bar(5);
//        System.out.println(var1.a);
//    }
//}
//
//class Foo {
//    int a = 5;
//
//    public boolean bar(int a) {
//        StringBuilder var1 = new StringBuilder();
//        var1.append(a);
//        String var2 = var1.toString();
//        System.out.println(var2);
//        return true;
//    }
//}

class Foo{
    int foo = 5;
}

class Bar extends Foo{

    Bar bar() {
        Bar bar = new Bar();
        foo = 3;
        return bar;
    }

//    int bar(int a, int b, int c) {
//        System.out.println(a);
//        System.out.println(b);
//        System.out.println(foo);
//        return c;
//    }
}
