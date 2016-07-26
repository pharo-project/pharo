accessing ports
text
	^ (self pane port: #text) value isNil 
		ifTrue: [self textBlock value]
		ifFalse: [(self pane port: #text) value]