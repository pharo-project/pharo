testing
testEquality
	| global1 global2 |
	
	global1 := RGGlobalVariableDefinition named: #Transcript.
	global2 := RGGlobalVariableDefinition named: #Transcript.
	self assert: global1 = global2.
	
	global2 := RGGlobalVariableDefinition named: #Transcript2.
	self deny: global1 = global2.