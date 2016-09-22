editing
insert: aString
	| operation |
	
	selection isEmpty ifFalse: [ 
		self delete.
		 ].
	
	operation := TxInsertOperation editor: self.
	operation text: aString.
	self addUndo: operation.
	
	selection replaceWith: aString.
	
	self notifyInserted: aString.
	
	"selection collapse."