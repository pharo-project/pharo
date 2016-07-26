testing - ast mapping
testBlockAndContextSourceNode
		
	|block blockNodeViaContext blockNodeViaClosure |


	block := [blockNodeViaContext := thisContext sourceNode].
	block value.
	blockNodeViaClosure := block sourceNode.

	self assert: blockNodeViaContext == blockNodeViaClosure
	

