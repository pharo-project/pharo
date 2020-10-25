Extension { #name : #UndefinedObject }

{ #category : #'*FluidClassBuilder' }
UndefinedObject >> < aString [

	^ FluidClassBuilder new
		  superclassToBuild: self;
		  classNameToBuild: aString;
		  yourself
]
