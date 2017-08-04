accessing nested
at: firstKey at: secondKey ifAbsent: aZeroArgBlock
	"Return the object stored in the second dictionary returned by firstKey and if absent execute the block argument. "
	| subDictionary |
	subDictionary := self at: firstKey ifAbsent: [ ^ aZeroArgBlock value ].
	^ subDictionary at: secondKey ifAbsent: aZeroArgBlock