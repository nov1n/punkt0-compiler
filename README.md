### Examples
The examples are in the folder `testprograms/`. Examples demonstrating the features of the compiler extension (foreign classes) are in the `testprograms/lab7` folder.

`constructor.p0` shows how punkt0 deals with foreign constructors with arguments. In particular we use java.io.File as an example.

`foregin.p0` uses java.util.Random to show how we can extend foreign classes, and override their methods.

### Running

To run first
```sbt compile```

Then ```sbt run -d ./classfiles testprograms/lab7/constructor.p0```

Classfiles are now in `classfiles` folder. These can be run with `java Main`