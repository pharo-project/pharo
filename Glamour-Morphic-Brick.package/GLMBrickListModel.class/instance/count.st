adaptor api
count

	^ countLogic
		ifNil: [ origin size ]
		ifNotNil: [ countLogic cull: origin ] 