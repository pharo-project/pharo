floating
layoutFloating: aBrick in: newBounds
	"must return an integer that will be added to the offset, if necessary"
	
	|float symbol|
	float := aBrick layout floating.
	symbol := self floatingMethodFor: float.
	
	^ (self respondsTo: symbol)
		ifTrue: [ self perform: symbol with: aBrick with: newBounds ]
		ifFalse: [ 0 ]