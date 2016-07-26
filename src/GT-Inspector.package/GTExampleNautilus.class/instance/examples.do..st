private
examples: aCollection do: aSelector
	^ aCollection size = 1
		ifTrue: [ aCollection anyOne perform: aSelector ]
		ifFalse: [ aCollection collect: aSelector ]