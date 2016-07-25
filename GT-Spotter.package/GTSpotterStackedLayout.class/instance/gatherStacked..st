as yet unclassified
gatherStacked: aBrick
	|list|
	
	list := DoubleLinkedList new.
	aBrick subbricks do: [ :each | each layoutProperties floating = #stack ifTrue: [ list add: each ] ].
	^ list