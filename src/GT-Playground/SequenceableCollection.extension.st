Extension { #name : #SequenceableCollection }

{ #category : #'*GT-Playground' }
SequenceableCollection >> takeFirst: anInteger [
	^ self first: (self size min: anInteger)
]
