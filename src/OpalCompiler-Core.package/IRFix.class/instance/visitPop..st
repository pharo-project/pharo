visiting
visitPop: pop
	prevInstr ifNil: [ ^ self ].
	prevInstr isStore ifFalse: [ ^ self ].
	"store then pop, however, cannot remove instr while iterating over the collection"
	storePopToFix add: prevInstr -> pop.
	
	