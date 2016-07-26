instance creation
named: infoName packageName: packageName spec: aMetacelloSpec
	"Create an instance of myself named infoName for the package named packageName"

	| answer |
	self assert: [ packageName isString ].
	self assert: [ packageName isSymbol not ].
	
	answer := super new.
	answer name: infoName.
	answer packageName: packageName.
	answer spec: aMetacelloSpec.
	^ answer