adding
addRubricItemsIn: aMenu

	aMenu addMorphBack: self rubricLabel.

	self isReadOnly 
		ifTrue: [ aMenu add: 'Be writable' selector: #beWriteable ]
		ifFalse: [ aMenu add: 'Be read only' selector: #beReadOnly ].
		
	aMenu add: 'Change background color' selector: #changeBackgroundColor.
	
	self isLineNumberingShown
		ifTrue: [ aMenu add: 'Hide line numbers' selector: #hideLineNumbers ]
		ifFalse: [ aMenu add: 'Show line numbers' selector: #showLineNumbers ]
