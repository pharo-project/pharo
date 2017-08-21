importing
importFor: requestor logSource: logSource
	
	(contents beginsWith: '----') ifTrue: [ ^self ].
	
	self flag: #fixme. "Ugly hack to parse preamble and postcript as a do it."
	((contents beginsWith: '"Change Set:') and: [ ChangeSet current preambleString isNil ])
		ifTrue: [ ChangeSet current preambleString: contents ].
	((contents beginsWith: '"Postscript:') and: [ ChangeSet current postscriptString isNil ])
		ifTrue: [ ChangeSet current postscriptString: contents ].

	^Smalltalk compiler class new
		source: contents;
		requestor: requestor;
		logged: logSource;
		evaluate.