Extension { #name : 'String' }

{ #category : '*Deprecated14' }
String >> asUrl [
	self
		deprecated: 'Use asZnUrl instead'
		in: #Pharo14
		transformWith: '`@rcv asUrl' -> '`@rcv asZnUrl'.
	^ self asZnUrl
]

{ #category : '*Deprecated14' }
String >> metacelloIntegerLessThanSelf: anInteger [
	"integer version components are always '>' string component"

	^false
]

{ #category : '*Deprecated14' }
String >> metacelloSemanticIntegerLessThanSelf: anInteger [
    "integer version components are always '>' string component"

    ^ true
]

{ #category : '*Deprecated14' }
String >> metacelloSemanticStringLessThanSelf: aString [
    ^ aString < self
]

{ #category : '*Deprecated14' }
String >> metacelloStringLessThanSelf: aString [

	^aString < self
]

{ #category : '*Deprecated14' }
String >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloStringLessThanSelf: self
]
