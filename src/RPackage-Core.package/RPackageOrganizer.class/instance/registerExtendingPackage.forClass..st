private registration
registerExtendingPackage: aPackage forClass: aClass 

	| cur |
	cur := classExtendingPackagesMapping at: aClass theNonMetaClass name ifAbsent: [nil].
	cur isNil
		ifTrue: [classExtendingPackagesMapping at: aClass theNonMetaClass name put: (Set with: aPackage)]
		ifFalse: [cur add: aPackage]