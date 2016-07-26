accessing
addPane: aPane
	aPane browser: self.
	self panes add: aPane.
	self hasStatusbar ifTrue: [
		self addDefaultStatusbarTransmissionFrom: aPane ].
	^ aPane