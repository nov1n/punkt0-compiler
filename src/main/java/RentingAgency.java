public class RentingAgency {
    String checkout(Vehicle v) {
        System.out.println("checkout");
        return "";
    }

    public RentingAgency() {
        Car car1 = new Car();
        System.out.println(this.checkout(car1));
    }

    public static void main(String[] args) {
        RentingAgency ra = new RentingAgency();
    }
}

class Vehicle {
    String checkout(Vehicle v) {
        System.out.println("checkout");
        return "";
    }
}

class Car extends Vehicle { }

