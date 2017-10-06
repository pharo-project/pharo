I know how to create classes in the system.
The classes are nor installed or modifies other objects. That is part of the job of the ShiftClassInstaller.

I can be extended by using a different builder enhancer. 
See ShDefaultBuilderEnhancer for a default implementation. 

I can be used directly to create anonymous classes, but it is better if you use the annonymous class installer.

I also can compare the old class with the configured new class to calculate the required changes.