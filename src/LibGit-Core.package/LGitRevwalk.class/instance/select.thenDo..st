walking
select: selectionBlock thenDo: actionBlock
	self do: [ :result | 
		(selectionBlock value: result) 
			ifTrue: [ actionBlock value: result ] ]