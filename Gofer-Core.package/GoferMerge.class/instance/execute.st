running
execute
	[ self model merge ]
		on: MCMergeResolutionRequest
		do: [ :request |  request autoMerge ].
	self gofer cleanup