queries
packageForBehavior: aClass
	"return a PDPackage and put in classToPackage"

	^ classToPackage
		at: aClass
		ifAbsentPut: [ 
			self packages
				detect: [ :each | each rPackageSet includesClass: aClass ]
				ifNone: [ self systemPackageContaining: aClass ] ]