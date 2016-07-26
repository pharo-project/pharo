widget package
selectedPackageIncludes: aClass
^ (self selectedPackage notNil) and:[
	 (self selectedPackage classes includes: aClass ) or:[
		self selectedPackage classes includes: aClass class ].
	]