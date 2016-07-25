accessing
parentOfClass: aClass
	"I can answer an RPackage or an RPackageTag, depending on the presence of Tags in package. 
	 This is an UI optimization to not show a tree RPackage->RPackageTag when there is just 
	 one tag in package, named in the same way as his parent"	

	| package tag |
	
	package := aClass package.
	tag := package classTagForClass: aClass.
	
	^ ((((tag name = package name) and: [ package classTags size <= 1 ]) or: [tag isNil])
		ifTrue: [ package ]
		ifFalse: [ tag ])
		asNautilusSelection