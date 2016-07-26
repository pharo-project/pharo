actions
handleMergeConflictDuring: aLoadBlock
	[aLoadBlock 
	on: MCMergeOrLoadWarning do: [ :mergeConflict | mergeConflict merge ]]
	on: MCMergeResolutionRequest do: [ :request |  request autoMerge ].