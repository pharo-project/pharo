private
suppressMergeDialogWhile: aBlock
	^aBlock 
		on: MCMergeResolutionRequest
		do: [:request | request autoMerge ]