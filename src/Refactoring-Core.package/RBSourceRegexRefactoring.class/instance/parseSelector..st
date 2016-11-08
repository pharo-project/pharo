private
parseSelector: aSelector
	^ [ RBParser parseMethodPattern: aSelector ]
		on: Error
		do: [ nil ]