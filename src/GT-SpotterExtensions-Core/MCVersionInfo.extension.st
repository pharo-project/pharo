Extension { #name : #MCVersionInfo }

{ #category : #'*GT-SpotterExtensions-Core-private' }
MCVersionInfo >> allAncestors [
	^ (self allAncestorsTo: OrderedCollection new)
		sort: [ :a :b | a timeStamp >= b timeStamp ]
]

{ #category : #'*GT-SpotterExtensions-Core-private' }
MCVersionInfo >> allAncestorsTo: aCollection [
	self ancestors do: [ :versionInfo | 
		" do not add/process ancestry-branches which have already been added/processed "
		(aCollection includes: versionInfo) ifFalse: [ 
			aCollection add: versionInfo.
			versionInfo allAncestorsTo: aCollection ] ].
	^ aCollection
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCVersionInfo >> gtDisplayOn: stream [
	stream nextPutAll: self name
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCVersionInfo >> spotterAncestorsFor: aStep [
	<spotterOrder: 1>
	self flag: 'rewrite for direct streaming - get rid of recursion'.
	aStep listProcessor
		title: 'Ancestors';
		allCandidates: [ self allAncestors ];
		filter: GTFilterSubstring
]
