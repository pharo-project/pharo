services
find
	| where |
	self setStartIndex.
	where := self findAndSelectRegex.
	where ifNil: [ self textArea flash ].
	^ where