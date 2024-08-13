Extension { #name : 'Behavior' }

{ #category : '*Shift-ClassBuilder' }
Behavior >> << aString [
	"Return a builder of class FluidClassBuilder. To get a class, this builder should be sent #build.
This way we can manipulate and test class definition without being forced to get them added to the environment."

	^ ShiftClassBuilder new
		  superclass: self;
		  name: aString;
		  fillClassSideFromEnvironment: self environment;
		  yourself
]

{ #category : '*Shift-ClassBuilder' }
Behavior >> fluidInstall [
	"Fluid class defintions return a builder object, while ST80 definitions install and return the class. By adding an empty method here we can create classes by evaluating the defintion and sending #installFluid for both cases. We can not use #install, as the method is used already"
]
