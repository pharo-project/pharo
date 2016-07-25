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
	
	self announce: (TxInsertedTextAnnouncement new insertedText: aString; editor: self; yourself).
	
	"selection collapse."