I had some problems using the 'ensure:' method. There was some conflicts with the organizer used for the test when trying to clean the classes, categories and packages generated for the test. 
There is something I don't get with this method. Therefore I choosed to clean by default all possible generated classes, categories and packages after each test using the tearDown method.

For now, all that was in the 'ensure:' methods has been commented.
Look for example at:
    testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsClass
and try to uncomment it