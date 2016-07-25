parenblinking
clearParens
	| lastLocation |
	(lastLocation := self editingState lastParenLocation) 
		ifNil: [^ self].
	self text size >= lastLocation 
		ifTrue: [ self text
			removeAttribute: TextEmphasis bold
			from: lastLocation
			to: lastLocation].
	self editingState lastParenLocation: nil