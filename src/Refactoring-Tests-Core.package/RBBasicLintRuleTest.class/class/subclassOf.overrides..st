private
subclassOf: aClass overrides: aSelector 
	| subs |
	subs := aClass subclasses.
	1 to: subs size
		do: 
			[:i | 
			| each |
			each := subs at: i.
			(each includesSelector: aSelector) ifTrue: [^true].
			(self subclassOf: each overrides: aSelector) ifTrue: [^true]].
	^false