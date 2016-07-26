tests
testProtocolRemoved
	| pass class classRemoved protocolRemoved |
	
	pass := false.
	
	SystemAnnouncer uniqueInstance 
		when: ProtocolRemoved do: [ :ann | 
			pass := true.
			classRemoved := ann classReorganized.			
			protocolRemoved := ann protocol ].
		
	class := factory newClass.
	class organization addCategory: 'shiny-new-category'.
	
	class organization removeCategory: 'shiny-new-category'.
		
	self assert: pass.
	self assert: classRemoved equals: class.
	self assert: protocolRemoved equals: 'shiny-new-category'