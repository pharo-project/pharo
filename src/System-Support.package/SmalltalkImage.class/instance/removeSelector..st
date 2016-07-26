shrinking
removeSelector: descriptor 
	"Safely remove a selector from a class (or metaclass). If the
	class or the method doesn't exist anymore, never mind and
	answer nil.
	This method should be used instead of 'Class removeSelector:
	#method' to omit global class references."
	| class sel |
	class := self
				at: descriptor first
				ifAbsent: [^ nil].
	(descriptor size > 2 and: [descriptor second == #class])
		ifTrue: 
			[class := class class.
			sel := descriptor third]
		ifFalse: 
			[sel := descriptor second].
	^ class removeSelector: sel