updating
recalculateIfInterestedInPackageNamed: packageName for: operation
	| recalculate |
	recalculate := super recalculateIfInterestedInPackageNamed: packageName for: operation.
	(recalculate not and: [ baselines notNil ])
		ifTrue: [ 
			(self baselines detect: [ :info | info recalculateIfInterestedInPackageNamed: packageName for: operation ] ifNone: [  ])
				ifNotNil: [ recalculate := true ] ].
	(recalculate not and: [ versions notNil ])
		ifTrue: [ 
			(self versions detect: [ :info | info recalculateIfInterestedInPackageNamed: packageName for: operation ] ifNone: [  ])
				ifNotNil: [ recalculate := true ] ].
	(recalculate and: [ operation ~~ #modified ])
		ifTrue: [ 
			currentVersion == #notLoaded
				ifTrue: [ currentVersion := nil ].
			self fullRecalculate ]