Extension { #name : #Behavior }

{ #category : #'*FluidClassBuilder' }
Behavior >> << aString [
	"Return a builder of class FluidClassBuilder. To get a class, this builder should be sent build.
This way we can manipulate and test class definition without be forced to get them added to the environment."
	
	^ FluidClassBuilder new
		  superclassToBuild: self;
		  classNameToBuild: aString;
		  yourself
]
