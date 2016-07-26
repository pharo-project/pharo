removing
fixPushNilsForTemps
	" There are pushConstant: nil in the beginning of the blocksequence for all of the defined temps.
	  We got these pushConstant: nil in. Now our closure will generate them again, meaning we will double
	  the number of pushConstant: nil in the final block. To avoid this, we strip the ones we got in from the IR.  "

	| blocksequence |
	blocksequence := self currentScope blockSequence sequence.
	self currentScope definedTemps do: [ :temp | blocksequence removeFirst ]. 
	