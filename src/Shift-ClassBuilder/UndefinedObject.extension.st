Extension { #name : #UndefinedObject }

{ #category : #'*Shift-ClassBuilder' }
UndefinedObject >> < aString [ 
	^ FluidClassBuilder new 
		superclassToBuild: self;
		classNameToBuild: aString;
		yourself
]
