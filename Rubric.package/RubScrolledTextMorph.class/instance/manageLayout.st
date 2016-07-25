layout
manageLayout
	| ret |
	self textArea ifNil: [ ^self ].
	ret := self manageLayoutInBounds: self innerBounds.
	self layoutChanged.
	^ret