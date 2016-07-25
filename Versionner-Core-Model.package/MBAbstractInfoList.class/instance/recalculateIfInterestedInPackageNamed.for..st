updating
recalculateIfInterestedInPackageNamed: packageName for: operation
	"keep projects separate, since I'm the only one interested in projects."
	(self interestedInPackageNamed: packageName)
		ifTrue: [ ^self interestedInPackage: operation ].
	projects notNil
		ifTrue: [ 
			(self projects detect: [ :info | info interestedInPackageNamed: packageName ] ifNone: [  ]) notNil
				ifTrue: [ ^self interestedInPackage: operation ] ].
	^false