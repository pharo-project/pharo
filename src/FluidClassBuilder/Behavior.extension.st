Extension { #name : #Behavior }

{ #category : #'*FluidClassBuilder' }
Behavior >> << aString [
	"Return a builder of class FluidClassBuilder. To get a class, this builder should be sent build.
This way we can manipulate and test class definition without be forced to get them added to the environment."
	
	^ FluidClassBuilder new
		  superclassToBuild: self;
		  nameToBuild: aString;
		  fillClassSideFromEnvironment: self environment;
		  yourself
]

{ #category : #'*FluidClassBuilder' }
Behavior >> install [
	"Fluid class defintions return a builder object, while ST80 definitions install and return the class. By adding an empty method here we can create classes by evaluating the defintion and sending #install for both cases"
]
