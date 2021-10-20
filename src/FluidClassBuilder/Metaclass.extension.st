Extension { #name : #Metaclass }

{ #category : #'*FluidClassBuilder' }
Metaclass >> << aMetaclass [
	"Return a builder of class FluidClassBuilder. To get a class, this builder should be sent build.
This way we can manipulate and test class definition without be forced to get them added to the environment."

	self assert: self soleInstance = aMetaclass soleInstance superclass.
	
	^ FluidClassBuilder new
		  superclassToBuild: self soleInstance;
		  nameToBuild: aMetaclass soleInstance name;
		  fillFromClass: aMetaclass soleInstance;
		  yourself
]
