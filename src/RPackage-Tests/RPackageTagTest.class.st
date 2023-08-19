"
SUnit tests for RPackage tags
"
Class {
	#name : #RPackageTagTest,
	#superclass : #RPackageTestCase,
	#category : #'RPackage-Tests'
}

{ #category : #running }
RPackageTagTest >> tearDown [

	#( TestClass TestClassOther ) do: [ :each | testEnvironment at: each ifPresent: #removeFromSystem ].
	super tearDown
]

{ #category : #tests }
RPackageTagTest >> testAddClass [

	| package1 package2 class |
	package1 := self createNewPackageNamed: #Test1.
	class := self createNewClassNamed: 'TestClass' inPackage: package1.

	self assert: (package1 includesClass: class).

	package2 := self createNewPackageNamed: #Test2.

	(package2 addClassTag: #TAG) addClass: class.

	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).
	self assert: (package2 hasTag: #TAG).
	self assert: ((package2 classTagNamed: #TAG) includesClass: class)
]

{ #category : #tests }
RPackageTagTest >> testAddClassFromTag [

	| package1 package2 class |
	package1 := self createNewPackageNamed: #Test1.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.

	self assert: (package1 includesClass: class).
	self assert: (package1 hasTag: #TAG1).
	self assert: ((package1 classTagNamed: #TAG1) includesClass: class).

	package2 := self createNewPackageNamed: #Test2.

	(package2 addClassTag: #TAG2) addClass: class.

	self deny: (package1 includesClass: class).
	self assert: (package2 includesClass: class).
	self assert: (package2 hasTag: #TAG2).
	self assert: ((package2 classTagNamed: #TAG2) includesClass: class)
]

{ #category : #tests }
RPackageTagTest >> testPromoteAsPackage [

	| package1 package2 class tag1 |
	package1 := self createNewPackageNamed: #Test1.
	class := self createNewClassNamed: 'TestClass' inCategory: 'Test1-TAG1'.
	class compile: 'foo ^42' classified: 'accessing'.

	tag1 := package1 classTagNamed: 'TAG1'.

	tag1 promoteAsPackage.

	package2 := self organizer packageNamed: 'Test1-TAG1'.
	self assert: package2 isNotNil.
	self assert: (package2 classes includes: class).
	self deny: (package1 classes includes: class)
]
