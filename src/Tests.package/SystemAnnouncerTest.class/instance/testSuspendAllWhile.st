tests
testSuspendAllWhile
	| value |
	
	value := 42.	
	
	SystemAnnouncer uniqueInstance suspendAllWhile: [ 
		SystemAnnouncer uniqueInstance 
			when: ClassAdded do: [ value := value + 1 ];
			announce: ClassAdded ].
		
	self assert: value = 42. "The answer is always 42 :)"
	
	SystemAnnouncer uniqueInstance announce: ClassAdded.
	
	self assert: value = 43.