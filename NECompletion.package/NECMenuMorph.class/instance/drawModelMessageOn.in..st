drawing
drawModelMessageOn: aCanvas in: rectangle 
	| message |
	
	self flag: #todo. "Removing for now... most of the shortcuts are broken and no time or need now to fix them" 

	message := context model hasMessage 
		ifTrue: [ context model message ", ' | ctrl-h=help'" ]
		ifFalse: [ self detailMessage ].
	aCanvas 
		drawString: message
		in: rectangle
		font: self class messageFont
		color: Color gray