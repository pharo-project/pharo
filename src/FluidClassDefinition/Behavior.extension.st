Extension { #name : #Behavior }

{ #category : #'*FluidClassDefinition' }
Behavior >> < aString [ 
	
	^ FluidClassBuilder new 
		superclassToBuild: self;
		classNameToBuild: aString;
		yourself
]
