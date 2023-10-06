Extension { #name : 'UndefinedObject' }

{ #category : '*FluidClassBuilder' }
UndefinedObject >> << aString [

	^ ShiftClassBuilder new
		  superclass: self;
		  name: aString;
		  yourself
]
