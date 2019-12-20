"
SUnit tests for RPackage extension methods
"
Class {
	#name : #RPackageExtensionMethodsSynchronisationTest,
	#superclass : #RPackageMCSynchronisationTest,
	#category : #'Monticello-Tests-RPackage'
}

{ #category : #'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryBestMatchingNameAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"

	| class XPackage YPackage |
	self addXYCategory.
	self addXMatchCategory.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX-YYYY'.

	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.

	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.

	self createMethodNamed: #newMethod inClass: class inCategory: '*XXXXX-YYYY'.

	self deny: (YPackage includesDefinedSelector: #newMethod ofClass: class).
	self assert: (XPackage includesExtensionSelector: #newMethod ofClass: class).

	self assert: (class >> #newMethod packageFromOrganizer: self organizer) equals: XPackage
]

{ #category : #'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryMatchingNameAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"

	| class XPackage YPackage |
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: #newMethod inClass: class inCategory: '*YYYYY-subcategory'.

	self assert: (YPackage includesExtensionSelector: #newMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #newMethod ofClass: class).

	self assert: (class >> #newMethod packageFromOrganizer: self organizer) equals: YPackage
]

{ #category : #testing }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryNotExistingCreateANewPackage [
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"
	| class firstPackage |
	
	self addXCategory.
	
	firstPackage := self organizer packageNamed: #XXXXX.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: #newMethod inClass:  class inCategory: '*SomethingDifferentNothingToDoWithWhatWeHave'.
	
	self deny: (class package includesDefinedSelector: #newMethod ofClass: class).
	
	self assert: (self organizer includesPackageNamed: #SomethingDifferentNothingToDoWithWhatWeHave).
]

{ #category : #testing }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryNotExistingCreateANewPackageAndInstallsMethodInIt [
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"
	| class firstPackage |
	
	self addXCategory.
	
	firstPackage := self organizer packageNamed: #XXXXX.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: #newMethod inClass:  class inCategory: '*SomethingDifferentNothingToDoWithWhatWeHave'.
	
	self deny: (class package includesDefinedSelector: #newMethod ofClass: class).
	
	self assert: (self organizer includesPackageNamed: #SomethingDifferentNothingToDoWithWhatWeHave).
	self assert: ((class >> #newMethod) packageFromOrganizer: self organizer ) equals: (self organizer packageNamed: #SomethingDifferentNothingToDoWithWhatWeHave).
]

{ #category : #'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryNotRespectingCaseAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with *)thae does not match the case of the corresponding package (for example *packagea for PackageA), this method is added to the corresponding extending package"
	
	| class firstPackage secondPackage|
	self addXYCategory.
	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: '*yyYyY'.
	
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) equals: secondPackage.
]

{ #category : #'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryWithExactMatchAddMethodToTheExtendingPackage [
	"test that when we add a method to a  class in an extension category ( beginning with *), this method is added to the corresponding extending package"

	| class firstPackage secondPackage |
	self addXYCategory.
	self addXMatchCategory.

	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.


	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*YYYYY'.

	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).

	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: secondPackage
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testModifyMethodByChangingCode [
	"test that when we modify the code of a method, everything work well: NOTHING SHOULD HAPPEN REGARDING THE PACKAGING"

	| class firstPackage |
	self addXCategory.
	firstPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class compile: 'stubMethod ^22222222222'.

	"nothing should change"
	self assert: (class >> #stubMethod) category equals: 'classic category'.
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: firstPackage
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testMoveClassInPackageWithExtensionsOnClass [
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."

	| class secondPackage |
	self addXYCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: secondPackage methodCategoryPrefix.

	secondPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension)."
	self assert: (class >> #stubMethod) category equals: Protocol defaultName.
	self assert: (secondPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: secondPackage
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testMoveClassInPackageWithExtensionsOnClass2 [
	"Move a class in package XXXXX (with extensions from XXXX) to package YYYYY."

	| class secondPackage |
	self addXYCategory.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*XXXXX'.

	secondPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension)."
	self assert: (class >> #stubMethod) category equals: Protocol defaultName.
	self assert: (secondPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: secondPackage
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testMoveClassInPackageWithExtensionsOnClassAndBack [
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."

	| class firstPackage secondPackage |
	self addXYCategory.
	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: secondPackage methodCategoryPrefix.

	secondPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension, but instead as 'as yet unclassified')."

	self assert: (class >> #stubMethod) category equals: Protocol defaultName.
	self assert: (secondPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod packageFromOrganizer: self organizer) equals: secondPackage.

	"Moving back, we should not see the extension reappear."

	firstPackage addClass: class.

	self assert: (class >> #stubMethod) category equals: Protocol defaultName.
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveAllExtensionMethodsFromMetaAndInstanceSideUpdateOrganizerExtensionsMapping [
	"test that In a class (from the meta  and the instance side ), when we remove all the method extended by the same external package, the class get unregistered from the classExtendingPackagesMapping of the organizer "

	|XPackage  class YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.

	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	self createMethodNamed: 'stubMethod2' inClass: class classSide  inCategory: '*yyyyy'. 

	class removeSelector: #stubMethod.
	class classSide removeSelector: #stubMethod2.
	"there should be no differences made between class and metaClass:"
	self deny: ((self organizer extendingPackagesOf: class) includes: YPackage). 
	self deny: ((self organizer extendingPackagesOf: class classSide) includes: YPackage). 
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveAllExtensionMethodsRemoveTheClassFromExtendedClasses [
	"test that In a class, when we remove all the method extended by the same external package, the class get unregistered from the extended classes of the external package "

	|XPackage  class YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	self createMethodNamed: 'stubMethod2' inClass: class  inCategory: '*yyyyy'. 
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClass: class). 
	self assert: (YPackage includesExtensionSelector: #stubMethod2 ofClass: class). 

	class removeSelector: #stubMethod.
	class removeSelector: #stubMethod2.
	self deny: (YPackage extendsClass: class).
	self deny: (YPackage extendsClass: class classSide).
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveExtensionMethodDoesNotRemoveExtendingPackage [
	"test that when we remove a method that is an extension from an external package, the package is not removed from the the class extending packages if another extension still exist"

	|XPackage  class YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	self createMethodNamed: 'stubMethod2' inClass: class  inCategory: '*yyyyy'. 
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClass: class). 
	self assert: (YPackage includesExtensionSelector: #stubMethod2 ofClass: class). 

	"If we only remove one of the extension methods, the package should still be extending the class."
	class removeSelector: #stubMethod.
	self assert: (YPackage extendsClass: class).
	self assert: (YPackage extendsClass: class classSide).
]

{ #category : #'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveExtensionMethodRemoveMethodFromItsPackage [
	"test that when we remove a method that is an extension from an external package, the method is removed from this package"

	|XPackage  class YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	
	class removeSelector: #stubMethod.
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class). 
]
