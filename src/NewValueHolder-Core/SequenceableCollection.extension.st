Extension { #name : #SequenceableCollection }

{ #category : #'*NewValueHolder-Core' }
SequenceableCollection >> asValueHolder [

	^ CollectionValueHolder value: self
]
