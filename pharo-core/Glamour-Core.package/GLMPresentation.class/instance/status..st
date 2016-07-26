accessing ports
status: anObject 
	self flag: 'what kind of transformation should we want here?'.
	(self pane port: #status) value: (self selectionTransformation glamourValue: anObject)