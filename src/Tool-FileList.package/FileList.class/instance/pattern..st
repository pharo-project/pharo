accessing
pattern: aStringOrNil
	pattern := aStringOrNil
		ifNotNil: [ :aString | 
			aString asString trimBoth
				ifEmpty: [ nil ]
				ifNotEmpty: [ :trimmed | trimmed ] ].
	self updateFileList