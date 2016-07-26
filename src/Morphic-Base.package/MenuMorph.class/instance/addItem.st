construction-UIManager
addItem

	| string sel |
	string := UIManager default request: 'Label for new item?'.
	string isEmpty ifTrue: [^ self].
	sel := UIManager default request: 'Selector?'.
	sel isEmpty ifFalse: [sel := sel asSymbol].
	self add: string selector: sel.
