Extension { #name : 'Behavior' }

{ #category : '*Shift-ClassBuilder' }
Behavior >> << aClassNameSymbol [
	"Return a <classBuilder> of class named <aClassNameSymbol>. To get a class, this builder should be sent the message `#build`.
This way we can manipulate and test class definition without being forced to get them added to the environment. If you are looking for a way to define classes in a playground or in a script use `<<<`"

	^ ShiftClassBuilder new
		  superclass: self;
		  name: aClassNameSymbol;
		  fillClassSideFromEnvironment: self environment;
		  yourself
]

{ #category : '*Shift-ClassBuilder' }
Behavior >> <<< aClassNameSymbol [
	"Define and install the named <aClassNameSymbol> in the system. 
	Return a class.
	Use this message for scripting and not for the class browser and other tools so that the tools can manipulate the class definition before installing it if they have to"
	
	^ (self << aClassNameSymbol) install
]

{ #category : '*Shift-ClassBuilder' }
Behavior >> fluidInstall [
	"Fluid class defintions return a builder object, while ST80 definitions install and return the class. By adding an empty method here we can create classes by evaluating the defintion and sending #installFluid for both cases. We can not use #install, as the method is used already"
]
