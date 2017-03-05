accessing
workspace: aWorkspace 
	workspace := aWorkspace.
	isForWorkspace ifNil: [ isForWorkspace := true ]