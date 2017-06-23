private
requestRemoteBranches
	^ self github
		requestBranches: self remote owner 
		project: self remote projectBasename