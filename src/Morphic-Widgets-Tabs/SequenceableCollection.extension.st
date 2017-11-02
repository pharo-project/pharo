Extension { #name : #SequenceableCollection }

{ #category : #'*Morphic-Widgets-Tabs' }
SequenceableCollection >> max: aBlock [
	| max |
	self ifEmpty: [ ^ nil ].
	max := aBlock value: self first.
	self
		allButFirstDo:
			[ :each | 
			| value |
			value := aBlock value: each.
			max := max max: value ].
	^ max
]
