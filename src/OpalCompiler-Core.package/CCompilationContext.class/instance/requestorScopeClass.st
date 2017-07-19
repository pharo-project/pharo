accessing
requestorScopeClass
	^ requestorScopeClass ifNil: [ requestorScopeClass := OCRequestorScope ]