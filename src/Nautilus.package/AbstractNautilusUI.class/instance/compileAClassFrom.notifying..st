source code area
compileAClassFrom: aString notifying: aController
	self sourceTextModel clearUserEdits.
	(self
		compileANewClassFrom: aString
		notifying: aController
		startingFrom: self selectedClass)
		ifNil: [ ^ self sourceTextModel hasUnacceptedEdits: true ]
		ifNotNil:
			[ :class | 
			self selectedPackage: (self parentOfClass: class).
			self selectedClass: class.
			self updatePackageGroupAndClassList ]