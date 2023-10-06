Extension { #name : 'UndefinedObject' }

{ #category : '*Shift-ClassBuilder' }
UndefinedObject >> << aString [

	^ ShiftClassBuilder new
		  superclass: self;
		  name: aString;
		  yourself
]
