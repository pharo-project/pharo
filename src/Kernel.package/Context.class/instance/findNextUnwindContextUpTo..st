private-exceptions
findNextUnwindContextUpTo: aContext
	"Return the next unwind marked above the receiver, returning nil if there is none.  Search proceeds up to but not including aContext."

	| context |
	<primitive: 195>
	context := self.
	[
		(context := context sender) == nil
		or: [ context == aContext ]
	] whileFalse: [
		context isUnwindContext
			ifTrue: [ ^context ]].
	^nil