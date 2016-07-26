accessing ports
selectedText
	^ (self pane port: #selectedText) value isNil 
		ifTrue: [self selectedTextBlock value]
		ifFalse: [(self pane port: #selectedText) value]