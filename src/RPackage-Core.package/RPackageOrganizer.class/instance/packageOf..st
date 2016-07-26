package access from class
packageOf: aClass

	^ classPackageMapping 
		at: aClass theNonMetaClass originalName
		ifAbsent: [self packageNamed: self packageClass defaultPackageName]