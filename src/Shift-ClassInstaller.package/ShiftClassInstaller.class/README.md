I am the responsible of installing a class in the system.

I interact with the ShiftClassBuilder to generate the classes.
You can use me as in:

Smalltalk classInstaller make: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #MyClass;
			slots: #(varA varB);
			category: 'My-Category' ].
		
See that I should never be referenced directly, only through the accesor 
in Smalltalk or in any class in the system. 

The block passed is used to configure the builder. Check ShiftClassBuilder to see the available messages.

I have a subclass to anonymous generate classes, without registering in the environment. 