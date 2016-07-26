tests
testProtocolAdded
	| pass class classReorganized protocolAdded |
	
	pass := false.
	
	SystemAnnouncer uniqueInstance 
		when: ProtocolAdded do: [ :ann | 
			pass := true.
			classReorganized := ann classReorganized.
			protocolAdded := ann protocol ].
		
	class := factory newClass.
	class organization addCategory: 'shiny-new-category'.
		
	self assert: pass.
	self assert: classReorganized equals: class.
	self assert: protocolAdded equals: 'shiny-new-category'