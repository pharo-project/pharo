Extension { #name : #Behavior }

{ #category : #'*FluidClassBuilder' }
Behavior >> < aString [ 
	
	^ FluidClassBuilder new 
		superclassToBuild: self;
		classNameToBuild: aString;
		yourself
]
