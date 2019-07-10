Extension { #name : #SpLabelledList }

{ #category : #'*Spec2-Deprecated' }
SpLabelledList >> filteringBlock: aBlock [
	self
		deprecated: 'This feature is now removed from Spec 2. If the visible elements of the list need to be changed, it''s the users of the lists that should manage it and update the list of items of the list.'
]
