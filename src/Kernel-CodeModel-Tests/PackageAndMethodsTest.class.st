"
SUnit tests for RPackage method synchronization
"
Class {
	#name : 'PackageAndMethodsTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testAddMethodInExtensionCategoryBestMatchingNameAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"

	| class xPackage yPackage tag |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	tag := self ensureTagYinX.

	self newClassNamed: 'NewClass' inTag: tag.
	class := self newClassNamed: 'NewClass' in: yPackage.

	self createMethodNamed: #newMethod inClass: class inProtocol: '*XXXXX-YYYY'.

	self deny: (yPackage includesSelector: #newMethod ofClass: class).
	self assert: (xPackage includesExtensionSelector: #newMethod ofClass: class).

	self assert: (class >> #newMethod) package equals: xPackage
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testAddMethodInExtensionCategoryMatchingNameAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: #newMethod inClass: class inProtocol: '*YYYYY-subcategory'.

	self assert: (yPackage includesExtensionSelector: #newMethod ofClass: class).
	self deny: (xPackage includesSelector: #newMethod ofClass: class).

	self assert: (class >> #newMethod) package equals: yPackage
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testAddMethodInExtensionCategoryNotExistingCreateANewPackage [
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"

	| class xPackage |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: #newMethod inClass: class inProtocol: '*SomethingDifferentNothingToDoWithWhatWeHave'.

	self deny: (class package includesSelector: #newMethod ofClass: class).

	self assert: (self organizer hasPackage: #SomethingDifferentNothingToDoWithWhatWeHave)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testAddMethodInExtensionCategoryNotExistingCreateANewPackageAndInstallsMethodInIt [
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"

	| class xPackage |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: #newMethod inClass: class inProtocol: '*SomethingDifferentNothingToDoWithWhatWeHave'.

	self deny: (class package includesSelector: #newMethod ofClass: class).

	self assert: (self organizer hasPackage: #SomethingDifferentNothingToDoWithWhatWeHave).
	self assert: (class >> #newMethod) package equals: (self organizer packageNamed: #SomethingDifferentNothingToDoWithWhatWeHave)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testAddMethodInExtensionCategoryNotRespectingCaseAddMethodToTheExtendingPackage [
	"test that when we add a method  in an extension category ( beginning with *)thae does not match the case of the corresponding package (for example *packagea for PackageA), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inProtocol: '*yyYyY'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).

	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testAddMethodInExtensionCategoryWithExactMatchAddMethodToTheExtendingPackage [
	"test that when we add a method to a  class in an extension category ( beginning with *), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	self ensureTagYinX.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inProtocol: '*YYYYY'.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).

	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'tests' }
PackageAndMethodsTest >> testAddRemoveMethod [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.

	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	self assert: (a2 >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self deny: (zPackage includesSelector: #methodDefinedInP1 ofClass: a2).

	a2 removeSelector: #methodDefinedInP2.
	self deny: (yPackage includesSelector: #methodDefinedInP2 ofClass: a2).

	a2 removeSelector: #methodDefinedInP1.
	self deny: (xPackage includesSelector: #methodDefinedInP3 ofClass: a2)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testExtensionMethods [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 1.
	self deny: (xPackage includesClass: a2).
	"method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , xPackage name.
	self assert: xPackage extensionSelectors size equals: 2
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testExtensionMethodsForClass [
	"test that when we add a method to a  class in an extension category ( beginning with *), this method is added to the corresponding extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	self ensureTagYinX.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inProtocol: '*YYYYY'.

	self assertEmpty: (xPackage extensionMethodsForClass: class).
	self assert: (yPackage extensionMethodsForClass: class) equals: { (class >> #stubMethod) }
]

{ #category : 'tests' }
PackageAndMethodsTest >> testMethodAddition [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1DefinedInX in: xPackage.
	a1 compileSilently: 'foo ^ 10'.
	xPackage addMethod: a1 >> #foo.
	self assert: (xPackage includesSelector: #foo ofClass: a1)
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testMethodPackageResolution [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A2InPackageP1 in: xPackage.
	a1 compile: 'method ^ #methodDefinedInP1'.
	a1 class compile: 'method ^ #methodDefinedInP1'.

	self assert: (a1 >> #method) package identicalTo: xPackage.
	self assert: (a1 class >> #method) package identicalTo: xPackage
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testModifyExtensionProtocolUpdatesThePackage [
	"test that when we move a method from an extension category ( begining with *) to another extending package , the method is moved from the extending package to the other extending package"

	| class xPackage yPackage zPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: #newMethod inClass: class asExtensionOf: yPackage.

	class classify: #newMethod under: '*zzzzz'.
	self deny: (xPackage includesSelector: #newMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (zPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (class >> #newMethod) package equals: zPackage
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testModifyMethodByChangingCode [
	"test that when we modify the code of a method, everything work well: NOTHING SHOULD HAPPEN REGARDING THE PACKAGING"

	| class xPackage |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'protocol'.

	class compile: 'stubMethod ^22222222222'.

	"nothing should change"
	self assert: (class >> #stubMethod) protocolName equals: 'protocol'.
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (xPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testModifyProtocolOfMethodyDoesNothing [
	"test that when we move a method from a classic category (not begining with *) to another classic category , the packaging keeps the same"

	| class xPackage |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'old protocol'.

	"this we do"
	class classify: #stubMethod under: 'new protocol'.

	"this we check"
	self assert: (class >> #stubMethod) protocolName equals: 'new protocol'.
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (xPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testModifyProtocolToExtensionUpdatesThePackage [
	"test that when we move a method from a classic category (not begining with *) to an extension category , the method is moved from the parent package of the class to the extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'old protocol'.

	class classify: #stubMethod under: '*yyyyy'.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: yPackage.

	class classify: #stubMethod under: '*yyyyy-subcategory'.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testModifyProtocolToRemoveExtensionUpdatesThePackage [
	"test that when we move a method from an extension category ( begining with *) to a classic category , the method is moved from the extending package to the parent package of the class"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class classify: #stubMethod under: 'classic protocol'.
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testMoveClassInPackageWithExtensionsOnClass [
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."

	| class yPackage xPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: '*' , yPackage name.

	yPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension)."
	self deny: (class >> #stubMethod) isClassified.
	self assert: (class >> #stubMethod) package equals: yPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testMoveClassInPackageWithExtensionsOnClassAndBack [
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: '*' , yPackage name.

	yPackage addClass: class.

	"Everything should now be in second package (and not listed as an extension, but instead as 'as yet unclassified')."
	self deny: (class >> #stubMethod) isClassified.
	self assert: (class >> #stubMethod) package equals: yPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class).

	"Moving back, we should not see the extension reappear."
	xPackage addClass: class.

	self deny: (class >> #stubMethod) isClassified.
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testPackageOfNonExtensionMethodIsPackageOfTheClass [
	"test that when we add a method to a  class in a classic category (not beginning with *), this method is added to the parent package of the class"

	| xPackage class |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'protocol'.

	self assert: (class >> #stubMethod) package equals: class package.
	self deny: (class >> #stubMethod) isExtension
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testPackageOfNonExtensionMethodIsPackageOfTheTrait [
	"test that when we add a method to a  trait in a classic category (*not beginning with *), this method is added to the parent package of the class"

	| xPackage trait |
	xPackage := self ensureXPackage.

	trait := self newTraitNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: trait inProtocol: 'protocol'.

	self assert: (trait >> #stubMethod) package equals: trait package.
	self deny: (trait >> #stubMethod) isExtension
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testRemoveAllExtensionMethodsFromMetaAndInstanceSideUpdateOrganizerExtensionsMapping [
	"test that In a class (from the meta  and the instance side ), when we remove all the method extended by the same external package, the class get unregistered from the classExtendingPackagesMapping of the organizer "

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.
	self createMethodNamed: 'stubMethod2' inClass: class classSide asExtensionOf: yPackage.

	class removeSelector: #stubMethod.
	class classSide removeSelector: #stubMethod2.
	"there should be no differences made between class and metaClass:"
	self deny: (class extendingPackages includes: yPackage).
	self deny: (class classSide extendingPackages includes: yPackage)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testRemoveAllExtensionMethodsRemoveTheClassFromExtendedClasses [
	"test that In a class, when we remove all the method extended by the same external package, the class get unregistered from the extended classes of the external package "

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.
	self createMethodNamed: 'stubMethod2' inClass: class asExtensionOf: yPackage.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).

	class removeSelector: #stubMethod.
	class removeSelector: #stubMethod2.
	self deny: (yPackage extendsClass: class).
	self deny: (yPackage extendsClass: class classSide)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testRemoveExtensionMethodDoesNotRemoveExtendingPackage [
	"test that when we remove a method that is an extension from an external package, the package is not removed from the the class extending packages if another extension still exist"

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.
	self createMethodNamed: 'stubMethod2' inClass: class asExtensionOf: yPackage.

	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod2 ofClass: class).

	"If we only remove one of the extension methods, the package should still be extending the class."
	class removeSelector: #stubMethod.
	self assert: (yPackage extendsClass: class).
	self assert: (yPackage extendsClass: class classSide)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testRemoveExtensionMethodRemoveMethodFromItsPackage [
	"test that when we remove a method that is an extension from an external package, the method is removed from this package"

	| xPackage class yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class removeSelector: #stubMethod.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - extension methods' }
PackageAndMethodsTest >> testRemoveExtensionMethodRemovesExtensionsFromPackage [

	| xPackage yPackage a1 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	"the class is created but not added to the package for now"
	a1 := self newClassNamed: #A1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 1.
	a1 compile: 'methodDefinedInP2 ^ #methodDefinedInP2' classified: '*' , yPackage name.

	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1).
	self assert: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a1).

	a1 removeSelector: #methodDefinedInP2.

	self deny: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1)
]

{ #category : 'tests - defined methods' }
PackageAndMethodsTest >> testRemoveMethodRemoveMethodFromItsPackage [
	"test that when we remove a method, the method is remod from the package in which the method is defined"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	class removeSelector: #stubMethod.

	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]
