public class Main {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder();
        sb.append(true);
        String str = sb.toString();
        System.out.println(str);
        Foo foo = new Foo();
        System.out.println(foo);
    }
}

class Foo {
    Foo() {
        Foo foo = null;
        System.out.println(foo);
        String s = "hey there!";
        System.out.println(s);
    }

}
