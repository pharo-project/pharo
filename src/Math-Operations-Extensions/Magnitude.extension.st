Extension { #name : #Magnitude }

{ #category : #'*Math-Operations-Extensions' }
Magnitude >> compareWith: anotherMagnitude ifLesser: lesserBlock ifEqual: equalBlock ifGreater: greaterBlock [

	^ self < anotherMagnitude
		ifTrue: lesserBlock
		ifFalse: [ 
			self = anotherMagnitude
				ifTrue: equalBlock
				ifFalse: greaterBlock ]
]
