private
createTheMethodReference: aModifiedMethodEvent
	
	| method |
	method := aModifiedMethodEvent method.

	^ (RGMethodDefinition
		realClass: method methodClass
		selector: method selector) asHistorical