Extension { #name : 'Object' }

{ #category : '*Collections-Sequenceable' }
Object >> join: aSequenceableCollection [
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	"(Character space join: #('Pharo' 'is' 'cool')) >>> 'Pharo is cool'"

	^ (Array with: self) join: aSequenceableCollection
]

{ #category : '*Collections-Sequenceable' }
Object >> split: aSequenceableCollection [
	"Split the argument using the receiver as a separator."
	"optimized version for single delimiters"
	"($/ split: '/foo/bar')>>>#('' 'foo' 'bar') asOrderedCollection"
	"([:c| c isSeparator] split: 'aa bb cc dd')>>> #('aa' 'bb' 'cc' 'dd') asOrderedCollection"

	| result |
	result := OrderedCollection new: aSequenceableCollection size //2.
	self split: aSequenceableCollection do: [ :item |
		result add: item ].
	^ result
]
