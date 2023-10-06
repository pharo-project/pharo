Extension { #name : 'UndefinedObject' }

{ #category : '*FluidClassBuilder' }
UndefinedObject >> << aString [

	^ FluidBuilder new
		  superclass: self;
		  nameToBuild: aString;
		  yourself
]
