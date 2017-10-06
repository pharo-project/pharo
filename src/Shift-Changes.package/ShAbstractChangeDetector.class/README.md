I detect the changes to a given Class.
My subclasses implements 

#compareClass: oldClass with: aBuilder to calculate the changes performed to the class.

I am responsible of adding the changes to the builder..

Check the initialization of the builder in the ShiftClassBuilder and in the ShDefaultBuilderEnhancer to see when the comparers are registered in the Shift class builder.