order
isActionHandled

	^ (method methodClass isMeta) and: [ 
		method selector = #initialize ] 