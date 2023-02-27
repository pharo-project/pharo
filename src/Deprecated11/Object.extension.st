Extension { #name : #Object }

{ #category : #'*Deprecated11' }
Object >> stepAt: millisecondClockValue in: aWindow [

	self deprecated: 'This method should be called on Model classes and not on any object.'.
	^ self stepIn: aWindow
]

{ #category : #'*Deprecated11' }
Object >> stepIn: aWindow [

	self deprecated: 'This method should be called on Model classes and not on any object.'.
	^ self step
]

{ #category : #'*Deprecated11' }
Object >> stepTime [

	self deprecated: 'This method should be called on Model or Morph classes and not on any object.'.
	^ 1000 "milliseconds -- default backstop for objects serving as models of system windows"
]

{ #category : #'*Deprecated11' }
Object >> stepTimeIn: aSystemWindow [

	self deprecated: 'This method should be called on Model classes and not on any object.'.
	^ self stepTime
]

{ #category : #'*Deprecated11' }
Object >> wantsStepsIn: aSystemWindow [

	self deprecated: 'This method should be called on Model classes and not on any object.'.
	^ self wantsSteps
]
