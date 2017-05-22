accessing
committish
	^ (self name includesSubstring: 'detached')
		ifTrue: [ ('\(detached from ([0-9a-f]+)\)' asRegex) matches: self name; subexpression: 2 ]
		ifFalse: [ self name ]