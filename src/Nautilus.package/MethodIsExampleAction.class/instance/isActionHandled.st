order
isActionHandled

	^  (method methodClass isMeta) and: [
			method numArgs isZero and: [   
				(method selector beginsWith: 'example') or: [ 
					method hasPragmaNamed: #example 
		]]]
	 