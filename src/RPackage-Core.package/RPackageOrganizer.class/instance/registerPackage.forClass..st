private registration
registerPackage: aPackage forClass: aClass 
	(aPackage includesClass: aClass) 
		ifFalse: 
			[self error: aPackage name , ' does not includes the class ' , aClass name].
	^classPackageMapping at: aClass theNonMetaClass name put: aPackage