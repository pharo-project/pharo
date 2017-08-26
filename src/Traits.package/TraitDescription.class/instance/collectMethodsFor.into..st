enquiries
collectMethodsFor: aSelector into: methodDescription
	(self includesSelector: aSelector) ifTrue: [ 
		methodDescription addLocatedMethod: 
			(self compiledMethodAt: aSelector)
		]