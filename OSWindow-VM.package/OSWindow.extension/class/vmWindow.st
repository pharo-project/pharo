*OSWindow-VM
vmWindow
	| attributes |
	^ VMWindow ifNil: [ 
		attributes := OSWindowAttributes new.
		attributes preferableDriver: VMWindowDriver new.
		attributes extent: 640@480.
		VMWindow := OSWindow createWithAttributes: attributes
	] 