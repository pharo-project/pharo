private
hasChildNodes: aNode
	| dependencies |
	
	[ dependencies := aNode dependencies ] 
		on: MessageNotUnderstood 
		do: [  ^false ].
	^ dependencies size > 0