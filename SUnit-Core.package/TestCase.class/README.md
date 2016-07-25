A TestCase is an implementation of the Command pattern to run a test.  TestCase instances are created with the class method #selector:, passing the symbol that names the method to be executed when the test case runs.  Various UIs exist to run these instances and they can also be created and run programmatically.

When you discover a new fixture, subclass TestCase and create a #test... method for the first test.  As that method develops and more #test... methods are added, you will find yourself refactoring temps into instance variables for the objects in the fixture and overriding #setUp to initialize these variables.  As required, override #tearDown to nil references, release objects and deallocate.

See my superclass' comment for assertion and logging information.