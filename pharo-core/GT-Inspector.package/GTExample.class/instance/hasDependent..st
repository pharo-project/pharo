testing
hasDependent: anExample
	(self traverser reversed
		action: [ :example | (example equalsExample: anExample) ifTrue: [ ^ true ] ];
		yourself) value.
	^ false