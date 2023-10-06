Extension { #name : 'UndefinedObject' }

{ #category : '*FluidClassBuilder' }
UndefinedObject >> << aString [

	^ FluidBuilder new
		  superclassToBuild: self;
		  nameToBuild: aString;
		  yourself
]
