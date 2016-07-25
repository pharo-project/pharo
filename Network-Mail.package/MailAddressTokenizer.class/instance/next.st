stream protocol
next
	| ans |
	cachedToken ifNil: [ ^self nextToken ].
	ans := cachedToken.
	cachedToken := nil.
	^ans