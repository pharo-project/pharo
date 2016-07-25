actions
merge: aVersion
	[[self basicMerge: aVersion]
		on: MCMergeResolutionRequest do: [:n | n merge ]]
			on: MCNoChangesException do: [:n | ]