*CodeExport
isLocalSelector: aSelector
	| method |
	method := self methodDict at: aSelector ifAbsent: [ ^ false ].
	^ method isFromTrait not