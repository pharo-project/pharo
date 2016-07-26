comparing
compareWith: anotherMagnitude ifLesser: lesserBlock ifEqual: equalBlock ifGreater: greaterBlock

	^ self < anotherMagnitude
		ifTrue: lesserBlock
		ifFalse: [ 
			self = anotherMagnitude
				ifTrue: equalBlock
				ifFalse: greaterBlock ]