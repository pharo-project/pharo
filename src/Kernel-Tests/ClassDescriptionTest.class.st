"
This is the unit test for the class ClassDescription. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #ClassDescriptionTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #coverage }
ClassDescriptionTest >> classToBeTested [

	^ ClassDescription
]

{ #category : #helpers }
ClassDescriptionTest >> compilationTestPackageName [
	^ 'TestKernel-TemporaryTestPackageForTest'
]

{ #category : #helpers }
ClassDescriptionTest >> createTestClass [

	^ ((Object << #TemporaryMockClass)
		   slots: {  };
		   package: self compilationTestPackageName) build
]

{ #category : #running }
ClassDescriptionTest >> tearDown [

	(RPackageOrganizer default packageNamed: self compilationTestPackageName ifAbsent: [ nil ])
		ifNotNil: [ :package | package removeFromSystem ].

	super tearDown
]

{ #category : #'tests - compilation' }
ClassDescriptionTest >> testAddSelectorWithMethodClassifyMethod [

	| class method |
	class := self createTestClass.
	class addSelector: #wammawink withMethod: (class compiler compile: 'wammawink ^ self').
	method := class >> #wammawink.

	self assert: method sourceCode equals: 'wammawink ^ self'.
	self assert: method protocol equals: Protocol unclassified.
	self assert: (class protocolOfSelector: #wammawink) name equals: Protocol unclassified
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testAllSlots [
	self assert: Context allSlots size equals: 6
]

{ #category : #'tests - compilation' }
ClassDescriptionTest >> testCompileClassified [

	| class method |
	class := self createTestClass.

	class compile: 'foo ^3' classified: #protocol.
	method := class >> #foo.

	self assert: method sourceCode equals: 'foo ^3'.
	self assert: method protocol equals: #protocol
]

{ #category : #'tests - compilation' }
ClassDescriptionTest >> testCompileClassifiedWithProtocolInstance [

	| class method protocol |
	class := self createTestClass.

	protocol := class ensureProtocol: #protocol.
	class compile: 'foo ^3' classified: protocol.
	method := class >> #foo.

	self assert: method sourceCode equals: 'foo ^3'.
	self assert: method protocol equals: protocol name
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testDefinesSlotNamed [
	self assert: (Point definesSlotNamed: #x).
	self deny: (Point definesSlotNamed: #z).

	self assert: (ExampleForTest11 definesSlotNamed: #aSubclassVariable).
	self deny: (ExampleForTest11 definesSlotNamed: #aSuperclassVariable)
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testDeprecatedAliases [

	| class systemDictionary |
	class := (Object << #TemporaryMockClass package: self compilationTestPackageName) install.
	systemDictionary := class environment.

	class deprecatedAliases: #( #TemporaryMockClassDeprecated #DeprecatedTemporaryMockClass ).

	self deny: (systemDictionary at: class name) isDeprecated.
	self assert: (systemDictionary lookupVar: #TemporaryMockClassDeprecated) isDeprecated.
	self assert: (systemDictionary lookupVar: #DeprecatedTemporaryMockClass) isDeprecated.

	"It should be possible to reaaply the code."
	class deprecatedAliases: #( #TemporaryMockClassDeprecated #DeprecatedTemporaryMockClass ).

	self deny: (systemDictionary at: class name) isDeprecated.
	self assert: (systemDictionary lookupVar: #TemporaryMockClassDeprecated) isDeprecated.
	self assert: (systemDictionary lookupVar: #DeprecatedTemporaryMockClass) isDeprecated
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testDeprecatedAliasesAreRemovedByClassRemoval [

	| class systemDictionary |
	class := (Object << #TemporaryMockClass package: self compilationTestPackageName) install.
	systemDictionary := class environment.

	class deprecatedAliases: #( #TemporaryMockClassDeprecated #DeprecatedTemporaryMockClass ).

	self assert: (systemDictionary includesKey: class name).
	self assert: (systemDictionary includesKey: #TemporaryMockClassDeprecated).
	self assert: (systemDictionary includesKey: #DeprecatedTemporaryMockClass).

	class removeFromSystem.

	self deny: (systemDictionary includesKey: class name).
	self deny: (systemDictionary includesKey: #TemporaryMockClassDeprecated).
	self deny: (systemDictionary includesKey: #DeprecatedTemporaryMockClass)
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testDeprecatedAliasesAreRemovedByClassRemovalSpecialCase [

	| class systemDictionary |
	class := ((Object << #TemporaryMockClass)
		          slots: {  };
		          package: self compilationTestPackageName) install.
	systemDictionary := class environment.

	class deprecatedAliases: #( #TemporaryMockClass2 ).

	self assert: (systemDictionary includesKey: class name).
	self assert: (systemDictionary includesKey: #TemporaryMockClass2).

	"Here is the trick, if someone change the global variable then we should not remove it with the class removal"
	[
	systemDictionary at: #TemporaryMockClass2 put: 1.
	class removeFromSystem.

	self deny: (systemDictionary includesKey: class name).
	self assert: (systemDictionary includesKey: #TemporaryMockClass2) ] ensure: [
		systemDictionary removeKey: #TemporaryMockClass2 ifAbsent: [ "Nothing it's just cleanup." ] ]
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testDeprecatedAliasesOnExistingNames [

	| class systemDictionary |
	class := (Object << #TemporaryMockClass package: self compilationTestPackageName) install.
	"Create a second class to register its name in the globals."
	(Object << #TemporaryMockClass2 package: self compilationTestPackageName) install.
	systemDictionary := class environment.

	self should: [ class deprecatedAliases: #( #TemporaryMockClass2 ) ] raise: Warning
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testHasSlot [
	self assert: (ExampleForTest11 hasSlot: (ExampleForTest11 slotNamed: #aSuperclassVariable)).
	self deny: (ExampleForTest11 definesSlot: (ExampleForTest11 slotNamed: #aSuperclassVariable))
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testHasSlotNamed [
	self assert: (Context hasSlotNamed: #sender).
	self assert: (Context hasSlotNamed: #method)
]

{ #category : #tests }
ClassDescriptionTest >> testMethods [
	self assert: Object methods equals: Object methodDict values
]

{ #category : #tests }
ClassDescriptionTest >> testNumberOfMethods [
	self assert: Point numberOfMethods equals: Point localMethods size + Point class localMethods size
]

{ #category : #tests }
ClassDescriptionTest >> testOrganization [

	| aClassOrganizer |
	aClassOrganizer := ClassDescription organization.
	self assert: (aClassOrganizer isKindOf: ClassOrganization)
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testSlotNamed [
	self assert: (Point slotNamed: #x) name equals: #x
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testSlotNames [
	self assert: Point slotNames equals: #(x y)
]

{ #category : #'tests - slots' }
ClassDescriptionTest >> testSlots [
	self assert: Context slots size equals: 6
]

{ #category : #'tests - instance variables' }
ClassDescriptionTest >> testclassThatDefinesInstVarNamed [
	self assert: (Point classThatDefinesInstVarNamed: 'x') equals: Point.
	self assert: (Class classThatDefinesInstVarNamed: 'methodDict') equals: Behavior.
	self assert: (Point classThatDefinesInstVarNamed: 'methodDict') equals: nil
]

{ #category : #'tests - instance variables' }
ClassDescriptionTest >> testwhichSelectorsAccess [
	self assert: ((Point whichSelectorsAccess: #y) includes: #y).
	self deny: ((Point whichSelectorsAccess: #y) includes: #x).
	self assert: ((Point whichSelectorsAccess: #x) includes: #setX:setY:).

	self assert: ((Point whichSelectorsAccess: #doesNotExist) isEmpty)
]

{ #category : #'tests - instance variables' }
ClassDescriptionTest >> testwhichSelectorsRead [
	self assert: ((Point whichSelectorsRead: #y) includes: #y).
	self deny: ((Point whichSelectorsRead: #y) includes: #x).

	self assert: ((Point whichSelectorsRead: #doesNotExist) isEmpty)
]

{ #category : #'tests - instance variables' }
ClassDescriptionTest >> testwhichSelectorsWrite [
	self assert: ((Point whichSelectorsWrite: #x) includes: #setX:setY:).
	self deny: ((Point whichSelectorsWrite: #x) includes: #x).

	self assert: ((Point whichSelectorsWrite: #doesNotExist) isEmpty)
]
