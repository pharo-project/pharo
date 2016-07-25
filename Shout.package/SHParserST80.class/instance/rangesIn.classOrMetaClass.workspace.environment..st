parse
rangesIn: sourceString classOrMetaClass: aBehaviour workspace: aWorkspace environment: anEnvironmentOrNil 
	anEnvironmentOrNil
		ifNotNil: [environment := anEnvironmentOrNil].
	self workspace: aWorkspace;
		 classOrMetaClass: aBehaviour;
		 source: sourceString.
	self parse.
	^ ranges