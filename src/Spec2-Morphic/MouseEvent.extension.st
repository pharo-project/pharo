Extension { #name : #MouseEvent }

{ #category : #'*Spec2-Morphic' }
MouseEvent >> isMiddleButton [

	^ self blueButtonPressed
]

{ #category : #'*Spec2-Morphic' }
MouseEvent >> isPrimaryButton [

	^ self redButtonPressed
]

{ #category : #'*Spec2-Morphic' }
MouseEvent >> isSecondaryButton [

	^ self yellowButtonPressed
]
