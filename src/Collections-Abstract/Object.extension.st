Extension { #name : #Object }

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> appendTo: aCollection [
	"double dispatch for join:"
	^ aCollection addLast: self
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> join: aSequenceableCollection [
	^ (Array with: self) join: aSequenceableCollection
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> joinTo: stream [
	"double dispatch for join:"
	^ stream nextPut: self
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> split: aSequenceableCollection [
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar'"
		
	| result position oldPosition |
	
	result := OrderedCollection new: (aSequenceableCollection size / 2) asInteger.
	self split: aSequenceableCollection do: [ :item |
		result add: item ].
	^ result

]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> split: aSequenceableCollection do: aBlock [
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar' indicesDo: [ :item | ]"
	self split: aSequenceableCollection indicesDo: [ :start :end | 
		aBlock value: (aSequenceableCollection copyFrom: start to: end) ]
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> split: aSequenceableCollection indicesDo: aBlock [
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar' indicesDo: [ :start :end | ]"
		
	|  position oldPosition |
	
	position := 1.
	oldPosition := position.
	
	position := aSequenceableCollection indexOf: self startingAt: position.
	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + 1.
		oldPosition := position.
		position := aSequenceableCollection indexOf: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size.

]
