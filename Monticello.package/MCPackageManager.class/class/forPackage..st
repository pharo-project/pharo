accessing
forPackage: aPackage
	^ self registry 
		at: aPackage 
		ifAbsent: [|mgr|
			mgr := self new initializeWithPackage: aPackage.
			self registry at: aPackage put: mgr.
			self announcer announce: (MCWorkingCopyCreated workingCopy: mgr package: aPackage).
			mgr ]