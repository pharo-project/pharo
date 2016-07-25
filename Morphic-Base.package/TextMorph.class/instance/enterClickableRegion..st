editing
enterClickableRegion: evt
	| |
	
	evt hand hasSubmorphs ifTrue:[^self].
	evt hand temporaryCursor ifNotNil:[^self].
	"paragraph ifNotNil:[
		(paragraph characterBlockAtPoint: evt position) stringIndex ].
"