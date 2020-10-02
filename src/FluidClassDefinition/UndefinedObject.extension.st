Extension { #name : #UndefinedObject }

{ #category : #'*FluidClassDefinition' }
UndefinedObject >> < aString [ 
	^ FluidClassBuilder new 
		superclassToBuild: self;
		classNameToBuild: aString;
		yourself
]
