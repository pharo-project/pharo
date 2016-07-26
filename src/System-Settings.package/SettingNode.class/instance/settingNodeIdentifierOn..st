printing
settingNodeIdentifierOn: aWriteStream 
	parentNode 
		ifNotNil: [ parentName settingNodeIdentifierOn: aWriteStream ]
		ifNil: [ parentName ifNotNil: [ aWriteStream print: parentName ] ].
	aWriteStream print: self settingDeclaration name