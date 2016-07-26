serialization
fuelAccept: aGeneralMapper

	^ index < 10
		ifTrue: [
			aGeneralMapper 
				visitSubstitution: self 
				by: (self copy index: self index + 1)
				onRecursionDo: [ super fuelAccept: aGeneralMapper ] ]
		ifFalse: [
			super fuelAccept: aGeneralMapper ]