drawing
layoutChanged
	
	self extent ~= layoutView extent ifTrue: [ layoutView extent: self extent ].
	super layoutChanged