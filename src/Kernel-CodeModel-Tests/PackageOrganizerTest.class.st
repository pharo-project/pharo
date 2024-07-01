"
RPackageOrganizerTest tests the behavior of PackageOrganizer. 
Note that it does not rely on announcement to be emitted to maintain invariants.
Therefore the new created PackageOrganizer is not registered to listen to event.


"
Class {
	#name : 'PackageOrganizerTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'utilities' }
PackageOrganizerTest class >> removeClassNamedIfExists: aClassNameSymbol [

	self environment at: aClassNameSymbol asSymbol ifPresent: [ :c | c removeFromSystem ]
]

{ #category : 'utilities' }
PackageOrganizerTest >> quadrangleClass [
	^ self organizer environment at: #QuadrangleForTesting
]

{ #category : 'tests' }
PackageOrganizerTest >> testAccessingPackage [

	| p1 |
	p1 := self ensurePackage: 'P1'.
	self assert: (self organizer packageNamed: #P1) equals: p1.
	self should: [ self organizer packageNamed: #P22 ] raise: Error
]

{ #category : 'tests' }
PackageOrganizerTest >> testCreateNewPackageWithConflictRaisesConflictException [

	self organizer addPackage: 'P1'.
	self should: [ self organizer addPackage: 'P1' ] raise: PackageConflictError
]

{ #category : 'tests' }
PackageOrganizerTest >> testCreateNewPackageWithoutConflictCreatesPackage [

	self organizer addPackage: 'P1'.
	self assert: (self organizer hasPackage: 'P1')
]

{ #category : 'tests' }
PackageOrganizerTest >> testDefaultEnvironment [
	"When no environment is specified, point to the default one"

	self assert: PackageOrganizer new environment identicalTo: Smalltalk globals
]

{ #category : 'tests' }
PackageOrganizerTest >> testDefaultOrganizer [

	self assert: self packageOrganizer identicalTo: self class environment organization
]

{ #category : 'tests' }
PackageOrganizerTest >> testDefinedClassesInstanceAndMetaSideAPI [

	| xPackage |
	xPackage := self ensureXPackage.
	self newClassNamed: #MyPoint in: xPackage.
	self assert: self organizer packageNames size equals: 2.
	self assert: self organizer packages size equals: 2.
	self assert: (self organizer packageNamed: self xPackageName) definedClasses size equals: 1
]

{ #category : 'tests' }
PackageOrganizerTest >> testEmpty [

	self assert: self organizer packageNames size equals: 1.
	self assert: (self organizer packageNames includes: UndefinedPackage undefinedPackageName)
]

{ #category : 'tests' }
PackageOrganizerTest >> testEnsurePackageManagesDifferentCase [

	| package1 package2 |
	package1 := self organizer ensurePackage: #Package1.
	package2 := self organizer ensurePackage: #PaCkaGe1.

	self assert: self organizer packages size equals: 2. "Unpackaged + Package1"
	self assert: package1 identicalTo: package2.
	self assert: package2 name equals: #Package1.
	self assert: (self organizer packageNamed: #Package1) identicalTo: package1.
	self assert: (self organizer packageNamed: #PaCKaGe1) identicalTo: package1.
	self assert: (self organizer packageNamed: #PackagE1) identicalTo: package1
]

{ #category : 'tests' }
PackageOrganizerTest >> testExtensionMethodNotExactlyTheName [

	| p1 p2 c1 |
	p1 := self ensurePackage: 'P1'.
	p2 := self ensurePackage: 'P2'.

	c1 := self newClassNamed: #C1 in: p2.

	c1 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*p1-something'.

	self deny: (self organizer hasPackage: #'p1-something').
	self assert: (p1 extendedClasses includes: c1)
]

{ #category : 'tests' }
PackageOrganizerTest >> testFullRegistration [

	| xPackage yPackage zPackage a1 a2 b1 b2 a3 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	a1 := self newClassNamed: #A1DefinedInX in: xPackage.
	b1 := self newClassNamed: #B1DefinedInX in: xPackage.
	a2 := self newClassNamed: #A2DefinedInY in: yPackage.
	b2 := self newClassNamed: #B2DefinedInY in: yPackage.
	a3 := self newClassNamed: #A3DefinedInZ in: zPackage.

	a1 compile: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.
	a2 class compile: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3' classified: '*' , zPackage name.

	self deny: (yPackage includesClass: b1).
	self assert: (yPackage includesClass: b2).
	"a locally defined class not extended by other packages"

	self assert: (yPackage includesClass: a2).
	"a locally defined class extended by other packages"

	self assert: (xPackage definesOrExtendsClass: a2).
	self deny: (xPackage includesClass: a2)
]

{ #category : 'tests' }
PackageOrganizerTest >> testHasPackage [

	| organizer packageFromOtherOrganizer name package |
	name := 'TestPackageNameForPackageOrganizer'.
	organizer := PackageOrganizer new.
	packageFromOtherOrganizer := PackageOrganizer new ensurePackage: name.
	self deny: (organizer hasPackage: name).
	self deny: (organizer hasPackage: packageFromOtherOrganizer).

	package := organizer ensurePackage: name.

	self assert: (organizer hasPackage: name).
	self assert: (organizer hasPackage: package).

	self deny: (organizer hasPackage: packageFromOtherOrganizer)
]

{ #category : 'tests' }
PackageOrganizerTest >> testHasPackageCaseInsensitive [

	self organizer ensurePackage: #Package1.

	self assert: (self organizer hasPackage: #Package1).
	self assert: (self organizer hasPackage: #PackaGe1).
	self deny: (self organizer hasPackage: #Packae1)
]

{ #category : 'tests' }
PackageOrganizerTest >> testPackageNamed [

	| package |
	package := self organizer ensurePackage: #Package1.

	self assert: self organizer packages size equals: 2. "Unpackaged + Package"
	self assert: package name equals: #Package1.
	self assert: (self organizer packageNamed: #Package1) identicalTo: package
]

{ #category : 'tests' }
PackageOrganizerTest >> testPackageNamedIfAbsent [

	| package |
	package := self organizer ensurePackage: #Package1.

	self assert: self organizer packages size equals: 2. "Unpackaged + Package"
	self assert: package name equals: #Package1.
	self assert: (self organizer packageNamed: #Package1 ifAbsent: [ self fail ]) identicalTo: package.
	self assert: (self organizer packageNamed: #Package ifAbsent: [ true ])
]

{ #category : 'tests' }
PackageOrganizerTest >> testPackageNamedIfPresent [

	| package |
	package := self organizer ensurePackage: #Package1.

	self assert: self organizer packages size equals: 2. "Unpackaged + Package"
	self assert: package name equals: #Package1.
	self assert: (self organizer packageNamed: #Package1 ifPresent: [ :package2 | package2 name ]) equals: package name.
	self assert: (self organizer packageNamed: #Package ifPresent: [ self fail ]) isNil
]

{ #category : 'tests' }
PackageOrganizerTest >> testPackageNamedWithDifferentCase [

	| package |
	package := self organizer ensurePackage: #Package1.

	self assert: self organizer packages size equals: 2. "Unpackaged + Package"
	self assert: package name equals: #Package1.
	self assert: (self organizer packageNamed: #PacKAge1) identicalTo: package
]

{ #category : 'tests' }
PackageOrganizerTest >> testPackageNamedWithoutMatchingPackage [

	self should: [ self organizer packageNamed: #PacKAge1 ] raise: NotFound
]

{ #category : 'tests' }
PackageOrganizerTest >> testPackageOf [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage.

	self assert: (self organizer packageOf: class) identicalTo: xPackage.

	yPackage := self ensureYPackage.
	yPackage addClass: class.

	self assert: (self organizer packageOf: class) identicalTo: yPackage
]

{ #category : 'tests' }
PackageOrganizerTest >> testRegisterPackageConflictWithPackage [

	self ensurePackage: 'P1'.
	self should: [ self organizer addPackage: (Package named: 'P1') ] raise: PackageConflictError
]

{ #category : 'tests' }
PackageOrganizerTest >> testRegisterPackageConflictWithPackageTag [
	"In the past we could not have package-tag with the same name than a package but now it is possible"

	| package1 package2 tag |
	package1 := self ensurePackage: 'P1'.
	tag := package1 ensureTag: #T1.

	package2 := self ensurePackage: #'P1-T1'.

	self assert: package2 name equals: #'P1-T1'.
	self assert: package1 name equals: #P1.
	self assert: tag name equals: #T1
]

{ #category : 'tests' }
PackageOrganizerTest >> testRegisterPackageTagConflictWithPackage [

	| package1 package2 tag |
	package1 := self ensurePackage: #P1.
	package2 := self ensurePackage: #'P1-T1'.
	tag := package1 ensureTag: #T1.

	self assert: package2 name equals: #'P1-T1'.
	self assert: package1 name equals: #P1.
	self assert: tag name equals: #T1
]

{ #category : 'tests' }
PackageOrganizerTest >> testRegisteredNumberOfPackageIsOk [
	| p1 p2 p3 |
	p1 := self ensurePackage: 'P1'.
	p2 := self ensurePackage: 'P2'.
	p3 := self ensurePackage: 'P3'.

	self organizer basicRegisterPackage: p1.
	self organizer basicRegisterPackage: p2.
	self organizer basicRegisterPackage: p3.
	self assert: self organizer packageNames size equals: 4.
	self organizer basicUnregisterPackage: p3.
	self assert: self organizer packageNames size equals: 3
]

{ #category : 'tests' }
PackageOrganizerTest >> testRegisteredPackages [

	| p1 p2 p3 |
	p1 := self ensurePackage: 'P1'.
	p2 := self ensurePackage: 'P2'.
	p3 := self ensurePackage: 'P3'.

	self assert: self organizer packageNames size equals: 4. "We also have the default package."
	{ p1 . p2 . p3 } do: [ :package |
		self assert: (self organizer packageNames includes: package name).
		self assert: (self organizer packages includes: package) ]
]

{ #category : 'tests' }
PackageOrganizerTest >> testRemoveClassUsingEnvironment [

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: #NewClass in: xPackage.

	self organizer environment removeClassNamed: #NewClass.
	self assert: (self organizer packageOf: class) isUndefined
]

{ #category : 'tests' }
PackageOrganizerTest >> testRemoveEmptyPackagesAndTags [

	| package1 package2 package3 package4 class tag1 tag2 |
	"This one will contain a class"
	package1 := self ensurePackage: #Test1.
	"This one will contain an extension method"
	package2 := self ensurePackage: #Test2.
	"This one will contain a tag with a class and an empty tag"
	package3 := self ensurePackage: #Test3.
	"This one will be empty"
	package4 := self ensurePackage: #Test4.

	tag1 := package3 ensureTag: #Tag1.
	tag2 := package3 ensureTag: #Tag2.

	class := self newClassNamed: 'TestClass' in: package1.
	class compile: 'extension ^ 1' classified: '*Test2'.

	class := self newClassNamed: 'TestClass2' inTag: tag1.

	self assert: (self organizer hasPackage: package1).
	self deny: package1 isEmpty.
	self assert: (self organizer hasPackage: package2).
	self deny: package2 isEmpty.
	self assert: (self organizer hasPackage: package3).
	self deny: package3 isEmpty.
	self deny: tag1 isEmpty.
	self assert: tag2 isEmpty.
	self assert: (package3 includesClassTagNamed: #Tag1).
	self assert: (package3 includesClassTagNamed: #Tag2).
	self assert: (self organizer hasPackage: package4).
	self assert: package4 isEmpty.

	self organizer removeEmptyPackagesAndTags.

	self assert: (self organizer hasPackage: package1).
	self assert: (self organizer hasPackage: package2).
	self assert: (self organizer hasPackage: package3).
	self assert: (package3 includesClassTagNamed: #Tag1).
	self deny: (package3 includesClassTagNamed: #Tag2).
	self deny: (self organizer hasPackage: package4)
]

{ #category : 'tests' }
PackageOrganizerTest >> testRemovePackage [

	| xPackage yPackage zPackage a1 a2 b1 b2 a3 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	a1 := self newClassNamed: #A1DefinedInX in: xPackage.
	b1 := self newClassNamed: #B1DefinedInX in: xPackage.
	a2 := self newClassNamed: #A2DefinedInY in: yPackage.
	b2 := self newClassNamed: #B2DefinedInY in: yPackage.
	a3 := self newClassNamed: #A3DefinedInZ in: zPackage.

	a1 compile: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.
	a2 class compile: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3' classified: '*' , zPackage name.

	self organizer removePackage: xPackage.
	self organizer removePackage: yPackage.
	self organizer removePackage: zPackage.

	self deny: (self organizer packageOf: a1) isNil.
	self deny: (self organizer packageOf: a2) isNil.
	self deny: (self organizer packageOf: b1) isNil.
	self deny: (self organizer packageOf: b2) isNil.
	self deny: (self organizer packageOf: a3) isNil
]

{ #category : 'tests' }
PackageOrganizerTest >> testRenameUpdateTheOrganizer [
	"test that when we rename a category, the organizer dictionary is update with this new name, so that we can access the package with this new name as key"

	| package |
	package := self ensurePackage: #Test1.

	self organizer renamePackage: package to: #Test2.
	self assert: package name equals: #Test2.
	self assert: (self organizer packageNamed: #Test2) identicalTo: package.
	self deny: (self organizer hasPackage: #Test1)
]

{ #category : 'tests' }
PackageOrganizerTest >> testTestPackageNames [

	| packages |
	packages := #( 'MockPackage-Tests' 'MockPackage2-tests' 'MockPackage' 'MockPackage-Tests-Package' ) collect: [ :pName | self ensurePackage: pName ].

	"Only 3 mock package names are test packages:  'MockPackage-Tests' 'MockPackage2-tests' 'MockPackage-Tests-Package'"
	self assert: self organizer testPackageNames size equals: 3.

	"Names of test packages are symbols."
	self assert: (self organizer testPackageNames allSatisfy: #isSymbol)
]

{ #category : 'tests' }
PackageOrganizerTest >> testTestPackages [

	| packages |
	packages := #( 'MockPackage-Tests' 'MockPackage2-tests' 'MockPackage' 'MockPackage-Tests-Package' ) collect: [ :pName | self ensurePackage: pName ].

	"Only 2 mock packages are test packages:  'MockPackage-Tests' 'MockPackage2-tests' 'MockPackage-Tests-Package'."
	self assert: self organizer testPackages size equals: 3.

	"all items from resulting collection are test packages."
	self assert: (self organizer testPackages allSatisfy: #isTestPackage)
]

{ #category : 'tests' }
PackageOrganizerTest >> testUnregister [

	| p1 p2 p3 |
	p1 := self ensurePackage: 'P1'.
	p2 := self ensurePackage: 'P2'.
	p3 := self ensurePackage: 'P3'.

	self assert: self organizer packageNames size equals: 4.

	{p1 . p2 . p3} do: [:package |
		(self organizer basicUnregisterPackage: package).
		self deny: (self organizer packageNames includes: package name) ].

	self assert: self organizer packageNames size equals: 1
]
