private
examples: aCollection
	^ aCollection size = 1
		ifTrue: [ aCollection anyOne ]
		ifFalse: [ aCollection ]