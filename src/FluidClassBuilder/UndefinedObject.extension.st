Extension { #name : 'UndefinedObject' }

{ #category : '*FluidClassBuilder' }
UndefinedObject >> << aString [

	^ FluidClassBuilder new
		  superclassToBuild: self;
		  nameToBuild: aString;
		  yourself
]
