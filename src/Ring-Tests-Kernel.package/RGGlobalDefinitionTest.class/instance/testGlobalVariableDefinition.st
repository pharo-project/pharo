testing
testGlobalVariableDefinition
	| global  |
	
	global:= RGGlobalVariableDefinition named:  #Transcript.
	self assert: (global isGlobalVariable).
	self assert: (global parent = Smalltalk globals).