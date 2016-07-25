updating
interestedInPackageNamed: packageName
	packages notNil
		ifTrue: [ 
			(self packages detect: [ :info | info interestedInPackageNamed: packageName ] ifNone: [  ]) notNil
				ifTrue: [ ^ true ] ].
	^ false