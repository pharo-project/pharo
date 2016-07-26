order
isActionHandled

	^  (method methodClass isMeta) and: [ 
		(method selector beginsWith: 'example')
			 and: [ method numArgs isZero ] ] 