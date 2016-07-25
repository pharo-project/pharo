testing
isActive: aContext

	^ aContext 
		ifNil: [ false ] 
		ifNotNil: [ 
			aContext isDead not and: [
				aContext ~= self process suspendedContext or: [aContext willReturn not ] ] ]