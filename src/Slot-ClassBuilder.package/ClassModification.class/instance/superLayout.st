accessing
superLayout
	^ superclass
		ifNil: [ EmptyLayout instance ]
		ifNotNil: [ superclass classLayout ]