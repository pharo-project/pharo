private
searchIndicesIn: anObject
	| class |
	class := self _objectClass: anObject.
	class isBits ifTrue: [^nil].
	class isWeak ifTrue: [^nil].
	1 to: (self _objectSize: anObject) do:
		[:i | | path |
		path := self processLinkTo: (self _object: anObject at: i) from: anObject.
		path notNil ifTrue: [^path]].
	^nil