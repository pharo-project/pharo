tests - operations on methods
testRemoveAllExtensionMethodsFromMetaAndInstanceSideUpdateOrganizerExtensionsMapping
	"test that In a class (from the meta  and the instance side ), when we remove all the method extended by the same external package, the class get unregistered from the classExtendingPackagesMapping of the organizer "

	|XPackage  class YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	self createMethodNamed: 'stubMethod2' inClass: class theMetaClass  inCategory: '*yyyyy'. 

	class removeSelector: #stubMethod.
	class theMetaClass removeSelector: #stubMethod2.
	"there should be no differences made between class and metaClass:"
	self deny: ((self organizer extendingPackagesOf: class) includes: YPackage). 
	self deny: ((self organizer extendingPackagesOf: class theMetaClass) includes: YPackage). 