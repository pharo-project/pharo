Class {
	#name : 'UndefinedClassTest',
	#superclass : 'TestCase',
	#category : 'UndefinedClasses-Tests',
	#package : 'UndefinedClasses-Tests'
}

{ #category : 'asserting' }
UndefinedClassTest >> assertClassDoesNotExist: aSymbol [

	self class environment at: aSymbol ifPresent: [ self fail ]
]

{ #category : 'asserting' }
UndefinedClassTest >> assertClassExist: aSymbol [

	self assert: (self class environment includesKey: aSymbol)
]

{ #category : 'utilities' }
UndefinedClassTest >> compile [
	<script: 'self new compile'>

	self createClassFromDefinitionString: self undefinedDefinition
]

{ #category : 'utilities' }
UndefinedClassTest >> createClassFromDefinitionString: aString [

	<script: 'self new compile'>
	^ ShiftClassInstaller make: [ :builder |
		  builder
			  useUndefinedClass;
			  buildFromAST: (CDFluidClassDefinitionParser parse: aString) ]
]

{ #category : 'asserting' }
UndefinedClassTest >> packageNameForTest [

	^ 'UndefinedClasses-Generated-Test-Package'
]

{ #category : 'running' }
UndefinedClassTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTest.
	self class environment at: #Foo ifPresent: [ :c | c removeFromSystem ].
	self class environment at: #ColoredFoo ifPresent: [ :c | c removeFromSystem ].
	super tearDown
]

{ #category : 'tests' }
UndefinedClassTest >> testClassWithUndefinedSuperclassNameHasUndefinedSuperclass [

	| class |
	self assertClassDoesNotExist: #Foo.

	class := self createClassFromDefinitionString: self undefinedDefinition.

	self assert: class superclass isUndefined
]

{ #category : 'tests' }
UndefinedClassTest >> testCreateSubclassOfArbitraryExpressionReturningNilThrowsError [

	self assertClassDoesNotExist: #ColoredFoo.

	self
		should: [
			self createClassFromDefinitionString: '(Array new: 1) first << #Box2 package: ''' , self packageNameForTest , '''' ]
		raise: Error.

	self assertClassDoesNotExist: #ColoredFoo
]

{ #category : 'tests' }
UndefinedClassTest >> testCreateSubclassOfNilCreatesSubclassOfNil [

	self assertClassDoesNotExist: #ColoredFoo.

	self createClassFromDefinitionString: 'nil << #ColoredFoo package: ''' , self packageNameForTest , ''''.

	self assertClassExist: #ColoredFoo.
	self assert: (self class environment at: #ColoredFoo) superclass equals: nil
]

{ #category : 'tests' }
UndefinedClassTest >> testCreateTwoUndefinedClassesOfSameNameShouldBeSameClass [

	| foo foo2 |
	self assertClassDoesNotExist: #Foo.

	foo := UndefinedClass createUndefinedClassNamed: #Foo package: self packageNameForTest.
	foo2 := UndefinedClass createUndefinedClassNamed: #Foo package: self packageNameForTest.

	self assert: foo identicalTo: foo2
]

{ #category : 'tests' }
UndefinedClassTest >> testCreateUndefinedClassShouldCreateNewClassThatIsUndefined [

	self assertClassDoesNotExist: #Foo.

	UndefinedClass createUndefinedClassNamed: #Foo package: self packageNameForTest.

	self assertClassExist: #Foo.
	self assert: [ (self class environment at: #Foo) isUndefined ]
]

{ #category : 'tests' }
UndefinedClassTest >> testDefinedClassIsUndefined [

	self deny: Object isUndefined
]

{ #category : 'tests' }
UndefinedClassTest >> testInstallClassNameWithUnknownSuperclassName [

	self assertClassDoesNotExist: #Foo.

	self createClassFromDefinitionString: self undefinedDefinition.

	self assertClassExist: #ColoredFoo.
	self assert: (self class environment at: #ColoredFoo) superclass equals: (self class environment at: #Foo).
	self assert: (self class environment at: #Foo) isUndefined
]

{ #category : 'tests' }
UndefinedClassTest >> testRemoveUndefinedClassShouldRemoveIt [

	self assertClassDoesNotExist: #Foo.

	UndefinedClass createUndefinedClassNamed: #Foo package: self packageNameForTest.

	self assertClassExist: #Foo.
	self assert: [ (self class environment at: #Foo) isUndefined ].

	(self class environment at: #Foo) removeFromSystem.

	self assertClassDoesNotExist: #Foo
]

{ #category : 'tests' }
UndefinedClassTest >> testUndefinedClassClassIsDefined [

	self assert: UndefinedClass isDefined
]

{ #category : 'tests' }
UndefinedClassTest >> testUndefinedClassIsUndefined [

	| undefinedClass |
	self assertClassDoesNotExist: #Foo.

	undefinedClass := UndefinedClass createUndefinedClassNamed: #Foo package: self packageNameForTest.
	self assert: undefinedClass isUndefined
]

{ #category : 'tests' }
UndefinedClassTest >> testUndefinedClassMetaclassIsDefined [

	self assert: UndefinedClass class isDefined
]

{ #category : 'tests' }
UndefinedClassTest >> testUndefinedMetaclassIsUndefined [

	| undefinedClass |
	self assertClassDoesNotExist: #Foo.

	undefinedClass := UndefinedClass createUndefinedClassNamed: #Foo package: self packageNameForTest.
	self assert: undefinedClass class isUndefined
]

{ #category : 'tests - handler' }
UndefinedClassTest >> testUnknowSuperclassShouldInheritFromUndefinedClass [
	| newUndefinedClass |
	self assertClassDoesNotExist: #Foo.

	self createClassFromDefinitionString: self undefinedDefinition.
	self assertClassExist: #ColoredFoo.
	newUndefinedClass := (self class environment at: #ColoredFoo).
	self assert: newUndefinedClass superclass equals: (self class environment at: #Foo).
	self assert: (self class environment at: #Foo) isUndefined
]

{ #category : 'accessing' }
UndefinedClassTest >> tokens [

	^ self undefinedDefinition splitOn: ' '
]

{ #category : 'accessing' }
UndefinedClassTest >> undefinedDefinition [

	^ 'Foo << #ColoredFoo package: ''' , self packageNameForTest , ''''
]
