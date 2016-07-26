initialization
initializeRule: aRule target: anEntity class: aClass selector: aSymbol

	self initializeRule: aRule target: anEntity.
	class := aClass.
	selector :=  aSymbol.
	
	tinyHint := class == sourceAnchor entity
		ifTrue: [ selector ]
		ifFalse: [ class name, '>>#', selector ]