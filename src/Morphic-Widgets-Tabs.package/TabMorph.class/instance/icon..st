protocol
icon: anIcon

	anIcon 
		ifNil: [ label icon: nil ]
		ifNotNil: [ label icon: (AlphaImageMorph new image: anIcon) ]