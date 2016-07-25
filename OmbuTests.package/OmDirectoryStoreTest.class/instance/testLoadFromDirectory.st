tests
testLoadFromDirectory

	| found |
	found := nil.

	store 
		entryFor: (OmReference globalName: #auxiliary localName: '1') 
		ifPresent: [ :entry | found := true ] 
		ifAbsent: [ found := false ].
		
	self assert: found