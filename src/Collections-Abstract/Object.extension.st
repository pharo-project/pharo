Extension { #name : #Object }

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> appendTo: aCollection [
	"double dispatch for join:"
	^ aCollection addLast: self
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> join: aSequenceableCollection [
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	"(Character space join: #('Pharo' 'is' 'cool')) >>> 'Pharo is cool'"
	
	^ (Array with: self) join: aSequenceableCollection
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> joinTo: stream [
	"double dispatch for join:"
	^ stream nextPut: self
]

{ #category : #'*Collections-Abstract-splitjoin' }
Object >> split: aSequenceableCollection [
	"Split the argument using the receiver as a separator."
	"optimized version for single delimiters"
	"($/ split: '/foo/bar')>>>#('' 'foo' 'bar') asOrderedCollection"
	"([:c| c isSeparator] split: 'aa bb cc dd')>>> #('aa' 'bb' 'cc' 'dd') asOrderedCollection"
		
	| result |
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
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of the receiver element that have been identified by splitting the receiver using the splitter argument. optimized version for single delimiters."
	
	"(String streamContents: [:s | Character space split: 'Pharo is cool'  indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:5 s:7 e:8 s:10 e:13 '"
		
		
		
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
