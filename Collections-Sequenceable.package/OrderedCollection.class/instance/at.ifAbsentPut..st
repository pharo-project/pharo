adding
at: index ifAbsentPut: block
	"Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)"

	| v |
	index <= self size ifTrue: [
		^ (v := self at: index)
			ifNotNil: [v]
			ifNil: [self at: index put: block value]
	].
	[self size < index] whileTrue: [self add: nil].
	^ self at: index put: block value