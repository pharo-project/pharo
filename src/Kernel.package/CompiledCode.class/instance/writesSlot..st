scanning
writesSlot: aSlot
	self ast nodesDo: [ :node | 
		node isVariable and: [
			node parent isAssignment and: [  
				(node parent variable = node) and: [  
					node isInstance and: [
						(node binding slot ==  aSlot)
					 		ifTrue: [^true]]]]]]. 
	^false