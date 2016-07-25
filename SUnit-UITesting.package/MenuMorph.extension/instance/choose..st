*SUnit-UITesting
choose: aString 

	| item |
	item := self itemWithWording: aString.
	item ifNil: [ self error: 'Menu does not have "', aString, '" item' ].
	item simulateClick.