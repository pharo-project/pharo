construction-UIManager
addTitle

	| string |
	string := UIManager default request: 'Title for this menu?'.
	string isEmptyOrNil ifTrue: [^ self].
	self addTitle: string.
