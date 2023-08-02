Extension { #name : #CompiledMethod }

{ #category : #'*Deprecated12' }
CompiledMethod >> isTaggedWith: aSymbol [

	self deprecated: 'You should compare the protocol instead of using this method.' transformWith: '`@rcv isTaggedWith: `@arg' -> '`@rcv protocolName == `@arg'.

	^ self protocolName == aSymbol
]

{ #category : #'*Deprecated12' }
CompiledMethod >> tagWith: aSymbol [

	self deprecated: 'Use #protocol: instead.' transformWith: '`@rcv tagWith: `@arg' -> '`@rcv protocol: `@arg'.
	self protocol: aSymbol
]

{ #category : #'*Deprecated12' }
CompiledMethod >> tags [
	"Any method could be tagged with multiple symbols for user purpose.
	For now we only define API to manage them implemented on top of method protocols.
	Protocol unclassified means that method is not tagged by anything"

	| protocol |
	self deprecated:
		'This method is missleading letting the user thing that there can be more than one protocol on a method. Most of the cases should just use #protocol(Name) and deal with the unclassified protocol if there is something special to do to it.'.
	protocol := self protocolName.
	protocol ifNil: [ ^ #(  ) ].
	protocol = Protocol unclassified ifTrue: [ ^ #(  ) ].
	^ { protocol }
]

{ #category : #'*Deprecated12' }
CompiledMethod >> untagFrom: aSymbol [

	self
		deprecated:
		'This method is missleading letting the user thing that there can be more than one protocol on a method. Most of the cases should just use #protocol: to update the protocol or #unclassify.'
		transformWith: '`@rcv untagFrom: `@arg' -> '`@rcv protocolName = `@arg ifTrue: [ `@rcv unclassify ]'.
	self protocolName = aSymbol ifTrue: [ self unclassify ]
]
