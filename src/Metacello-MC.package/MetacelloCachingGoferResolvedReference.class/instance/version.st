accessing
version
	"Answer a Monticello version of the receiver."

	cachedVersion == nil ifTrue: [ cachedVersion := super version ].
	^cachedVersion