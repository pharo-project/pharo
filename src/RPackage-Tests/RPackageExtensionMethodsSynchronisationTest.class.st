"
SUnit tests for RPackage extension methods
"
Class {
	#name : 'RPackageExtensionMethodsSynchronisationTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryBestMatchingNameAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"

	| class xPackage yPackage tag |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	tag := self ensureTagYinX.

	self newClassNamed: 'NewClass' inTag: tag.
	class := self newClassNamed: 'NewClass' in: yPackage.

	self createMethodNamed: #newMethod inClass: class inCategory: '*XXXXX-YYYY'.

	self deny: (yPackage includesDefinedSelector: #newMethod ofClass: class).
	self assert: (xPackage includesExtensionSelector: #newMethod ofClass: class).

	self assert: (class >> #newMethod) package equals: xPackage
]

{ #category : 'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryMatchingNameAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: #newMethod inClass: class inCategory: '*YYYYY-subcategory'.

	self assert: (yPackage includesExtensionSelector: #newMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #newMethod ofClass: class).

	self assert: (class >> #newMethod) package equals: yPackage
]

{ #category : 'testing' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryNotExistingCreateANewPackage [
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"

	| class xPackage |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: #newMethod inClass: class inCategory: '*SomethingDifferentNothingToDoWithWhatWeHave'.

	self deny: (class package includesDefinedSelector: #newMethod ofClass: class).

	self assert: (self organizer hasPackage: #SomethingDifferentNothingToDoWithWhatWeHave)
]

{ #category : 'testing' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryNotExistingCreateANewPackageAndInstallsMethodInIt [
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"

	| class xPackage |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: #newMethod inClass: class inCategory: '*SomethingDifferentNothingToDoWithWhatWeHave'.

	self deny: (class package includesDefinedSelector: #newMethod ofClass: class).

	self assert: (self organizer hasPackage: #SomethingDifferentNothingToDoWithWhatWeHave).
	self assert: (class >> #newMethod) package equals: (self organizer packageNamed: #SomethingDifferentNothingToDoWithWhatWeHave)
]

{ #category : 'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryNotRespectingCaseAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with *)thae does not match the case of the corresponding package (for example *packagea for PackageA), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyYyY'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class).

	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'we are not sure' }
RPackageExtensionMethodsSynchronisationTest >> testAddMethodInExtensionCategoryWithExactMatchAddMethodToTheExtendingPackage [
	"test that when we add a method to a  class in an extension category ( beginning with *), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	self ensureTagYinX.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*YYYYY'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesDefinedSelector: #stubMethod ofClass: class).

	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testModifyMethodByChangingCode [
	"test that when we modify the code of a method, everything work well: NOTHING SHOULD HAPPEN REGARDING THE PACKAGING"

	| class xPackage |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class compile: 'stubMethod ^22222222222'.

	"nothing should change"
	self assert: (class >> #stubMethod) protocolName equals: 'classic category'.
	self assert: (xPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: xPackage
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testMoveClassInPackageWithExtensionsOnClass [
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."

	| class yPackage xPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*' , yPackage name.

	yPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension)."
	self deny: (class >> #stubMethod) isClassified.
	self assert: (yPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testMoveClassInPackageWithExtensionsOnClassAndBack [
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."

	| class xPackage secondPackage |
	xPackage := self ensureXPackage.
	secondPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*' , secondPackage name.

	secondPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension, but instead as 'as yet unclassified')."

	self deny: (class >> #stubMethod) isClassified.
	self assert: (secondPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: secondPackage.

	"Moving back, we should not see the extension reappear."

	xPackage addClass: class.

	self deny: (class >> #stubMethod) isClassified.
	self assert: (xPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveAllExtensionMethodsFromMetaAndInstanceSideUpdateOrganizerExtensionsMapping [
	"test that In a class (from the meta  and the instance side ), when we remove all the method extended by the same external package, the class get unregistered from the classExtendingPackagesMapping of the organizer "

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.
	self createMethodNamed: 'stubMethod2' inClass: class classSide inCategory: '*yyyyy'.

	class removeSelector: #stubMethod.
	class classSide removeSelector: #stubMethod2.
	"there should be no differences made between class and metaClass:"
	self deny: ((self organizer extendingPackagesOf: class) includes: yPackage).
	self deny: ((self organizer extendingPackagesOf: class classSide) includes: yPackage)
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveAllExtensionMethodsRemoveTheClassFromExtendedClasses [
	"test that In a class, when we remove all the method extended by the same external package, the class get unregistered from the extended classes of the external package "

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.
	self createMethodNamed: 'stubMethod2' inClass: class inCategory: '*yyyyy'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).

	class removeSelector: #stubMethod.
	class removeSelector: #stubMethod2.
	self deny: (yPackage extendsClass: class).
	self deny: (yPackage extendsClass: class classSide)
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveExtensionMethodDoesNotRemoveExtendingPackage [
	"test that when we remove a method that is an extension from an external package, the package is not removed from the the class extending packages if another extension still exist"

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.
	self createMethodNamed: 'stubMethod2' inClass: class inCategory: '*yyyyy'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).

	"If we only remove one of the extension methods, the package should still be extending the class."
	class removeSelector: #stubMethod.
	self assert: (yPackage extendsClass: class).
	self assert: (yPackage extendsClass: class classSide)
]

{ #category : 'tests - operations on methods' }
RPackageExtensionMethodsSynchronisationTest >> testRemoveExtensionMethodRemoveMethodFromItsPackage [
	"test that when we remove a method that is an extension from an external package, the method is removed from this package"

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class removeSelector: #stubMethod.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]
