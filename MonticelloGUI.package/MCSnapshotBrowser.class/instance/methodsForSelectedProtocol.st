accessing
methodsForSelectedProtocol
	| methods |
	protocolSelection ifNil: [^ Array new].
	methods := self methodsForSelectedClass asOrderedCollection.
	(protocolSelection = '-- all --') 
		ifFalse: [methods removeAllSuchThat: [:ea | ea category ~= protocolSelection]].
	^ methods 
	
								