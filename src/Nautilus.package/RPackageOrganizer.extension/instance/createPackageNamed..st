*Nautilus
createPackageNamed: aString

	| instance |
	(packages includesKey: aString asSymbol) ifTrue: [ 
			self openAlreadyExistingError.
			^ nil ].
	
	instance := self packageClass named: aString.
	self registerPackage: instance.
	^ instance