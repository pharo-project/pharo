selection
selectedMethodIndex
	| methodClass selector |
	self selectedMethod ifNil: [ ^ 0 ].
	methodClass := self selectedMethod methodClass.
	selector := self selectedMethod selector.
	^ self getMethods detectIndex: [ :method | method methodClass = methodClass and: [ method selector = selector ] ] ifNone: [ 0 ]