layout
layoutChanged 
	"react on morph resize"
	super layoutChanged.
	surface ifNotNil: [  
		self extent asIntegerPoint ~= surface extent ifTrue: [ 
			self createSurface ]
	].
	