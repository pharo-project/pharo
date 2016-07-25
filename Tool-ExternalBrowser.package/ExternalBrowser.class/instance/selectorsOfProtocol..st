structure accessing
selectorsOfProtocol: aProtocol 
	
	| theSelectedClass |
	theSelectedClass := self showClassSide state
		ifFalse: [ self classes selectedItem ]
		ifTrue: [ self classes selectedItem theMetaClass ].
	^ (theSelectedClass 
		methodsInProtocol: self protocols selectedItem) collect: #selector