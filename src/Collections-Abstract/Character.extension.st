Extension { #name : #Character }

{ #category : #'*Collections-Abstract-splitjoin' }
Character >> join: aSequenceableCollection [
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	"(Character space join: #('Pharo' 'is' 'cool')) >>> 'Pharo is cool'"
	^ self asString join: aSequenceableCollection

]
