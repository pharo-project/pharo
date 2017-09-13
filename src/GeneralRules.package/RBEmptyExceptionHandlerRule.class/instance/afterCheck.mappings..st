hooks
afterCheck: aNode mappings: mappingDict
	| exception class |
	exception := mappingDict at: '`exception'.
	
	exception isVariable ifFalse: [ ^ false ].
	(class := Smalltalk classNamed: exception name) ifNil: [ ^ false ].
	(class includesBehavior: Exception) ifFalse: [ ^ false ].
	(class includesBehavior: Notification) ifTrue: [ ^false ].
	
	^ true