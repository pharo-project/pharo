Extension { #name : 'SmalltalkImage' }

{ #category : '*Deprecated14' }
SmalltalkImage >> os [
	"Answer the object to query about os."

	self deprecated: 'Use `OSPlatform current` instead' transformWith: '`@rcv os' -> 'OSPlatform current'.
	^ OSPlatform current
]

{ #category : '*Deprecated14' }
SmalltalkImage >> platform [
	"Answer the object to query about os."
	self deprecated: 'Use `OSPlatform current` instead'.
	^ OSPlatform current
]

{ #category : '*Deprecated14' }
SmalltalkImage >> platformName [

	self deprecated: 'Use `OSPlatform current platformName` instead'.
	^ OSPlatform current platformName
]

{ #category : '*Deprecated14' }
SmalltalkImage >> version [
	"Answer the version of this release."

	self deprecated: 'Use `SystemVersion current` instead'.

	^ SystemVersion current
		  ifNil: [ 'No version set yet' ]
		  ifNotNil: [ :version | version version ]
]
