Extension { #name : #SequenceableCollection }

{ #category : #'*NewValueHolder' }
SequenceableCollection >> asValueHolder [

	^ CollectionValueHolder value: self
]
