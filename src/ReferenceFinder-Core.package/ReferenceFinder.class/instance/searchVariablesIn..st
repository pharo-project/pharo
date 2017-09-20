private
searchVariablesIn: anObject
	| class |
	class := self _objectClass: anObject.
	1 to: class instSize do:
		[:i | | path |
		path := self processLinkTo: (self _object: anObject instVarAt: i) from: anObject.
		path notNil ifTrue: [^path]].
	^nil