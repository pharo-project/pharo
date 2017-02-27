testing
canExpand
	^complexContents 
		ifNotNil: [ complexContents hasContents ]
		ifNil: [ false ]