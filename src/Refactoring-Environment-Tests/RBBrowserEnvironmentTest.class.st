"
Unit tests for RBBrowserEnvironment and its wrappers.

Most checks walk their whole universe many times, so `universalEnvironment` is an `RBScopedBrowserEnvironment` limited to the classes of #universeClasses. The tests that need the whole image set `universalEnvironment` to an `RBBrowserEnvironment` themselves.
"
Class {
	#name : 'RBBrowserEnvironmentTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'universalEnvironment'
	],
	#classVars : [
		'AClassVariable',
		'ClassSideOnlyVariable'
	],
	#category : 'Refactoring-Environment-Tests',
	#package : 'Refactoring-Environment-Tests'
}

{ #category : 'accessing' }
RBBrowserEnvironmentTest class >> accessFromClassSideOnlyVariable [
	"testing method. This class variable is only accessed from this class side method"

	^ ClassSideOnlyVariable
]

{ #category : 'accessing' }
RBBrowserEnvironmentTest class >> defaultTimeLimit [
	^30 seconds
]

{ #category : 'accessing' }
RBBrowserEnvironmentTest class >> packageNamesUnderTest [
	^ #('Refactoring-Environment')
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> classNamesFor: anEnvironment [
	| classNames allClassNames |
	classNames := IdentitySet new
		addAll: anEnvironment classNames asSet;
		addAll: anEnvironment not classNames;
		yourself.
	allClassNames := universalEnvironment classNames asSortedCollection.
	self assert: classNames asSortedCollection equals: allClassNames.
	self assertEmpty: (anEnvironment & anEnvironment not) classNames.
	self assert: (anEnvironment | anEnvironment not) classNames asSortedCollection equals: allClassNames
]

{ #category : 'mockup' }
RBBrowserEnvironmentTest >> classVariableReader [
	^ AClassVariable
]

{ #category : 'mockup' }
RBBrowserEnvironmentTest >> classVariableWriter [
	AClassVariable := nil
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> classesFor: aBrowserEnvironment [
	| allClasses |
	allClasses := aBrowserEnvironment classes.
	allClasses addAll: aBrowserEnvironment not classes.
	universalEnvironment allClassesDo: [ :each | allClasses remove: each ].
	self assertEmpty: allClasses
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> copyFor: aBrowserEnvironment [
	| newEnvironment |
	newEnvironment := aBrowserEnvironment copy.
	self assert: newEnvironment numberSelectors equals: aBrowserEnvironment numberSelectors.
	self assert: (newEnvironment not & aBrowserEnvironment) numberSelectors equals: 0
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> keysFor: aBrowserEnvironment [
	| allKeys |
	allKeys := IdentitySet withAll: aBrowserEnvironment keys.
	allKeys addAll: aBrowserEnvironment not keys.
	allKeys removeAll: testingEnvironment keys.
	self assertEmpty: allKeys
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> labelFor: aBrowserEnvironment [
	self deny: aBrowserEnvironment label isNil.
	self deny: aBrowserEnvironment label isEmpty.
	self deny: aBrowserEnvironment defaultLabel isNil.
	self deny: aBrowserEnvironment defaultLabel isEmpty
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> numberSelectorsFor: aBrowserEnvironment [
	| numberSelectors numberSelectorsNot |
	numberSelectors := aBrowserEnvironment numberSelectors.
	numberSelectorsNot := aBrowserEnvironment not numberSelectors.
	self assert: numberSelectors + numberSelectorsNot equals: universalEnvironment numberSelectors.
	self assert: (aBrowserEnvironment & aBrowserEnvironment not) numberSelectors equals: 0.
	self assert: (universalEnvironment & aBrowserEnvironment) numberSelectors equals: numberSelectors.
	self assert: (aBrowserEnvironment & universalEnvironment) numberSelectors equals: numberSelectors
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> packageClassNames [

	^ (self class package definedClasses collect: [ :class | class name ]) asSet
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> packagesFor: anEnvironment [

	| allPackages |
	allPackages := IdentitySet withAll: universalEnvironment packages.
	allPackages removeAll: anEnvironment packages.
	anEnvironment not packages do: [ :package | allPackages remove: package ifAbsent: [  ] ].
	allPackages do: [ :package | self assertEmpty: (universalEnvironment classesInPackage: package) ]
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> protocolsFor: aBrowserEnvironment [

	aBrowserEnvironment classesAndSelectorsDo: [ :class :selector |
		self assert: (aBrowserEnvironment includesProtocol: (class protocolOfSelector: selector) in: class) ]
]

{ #category : 'running' }
RBBrowserEnvironmentTest >> setUp [
	super setUp.
	universalEnvironment := RBScopedBrowserEnvironment onClasses: self universeClasses.
	testingEnvironment := universalEnvironment systemDictionary
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testAndAnOrEnvironmentAndAnotherOrEnvironment [

	| orEnvironmentWithPackage orEnvironmentWithClass andEnvironment newClass |
	universalEnvironment := RBBrowserEnvironment new.
	"The first or environment contains a single package"
	orEnvironmentWithPackage := (universalEnvironment forClasses: {  })
	                            |
	                            (universalEnvironment forPackages:
		                             { self class package }).
	orEnvironmentWithClass := (universalEnvironment forClasses:
		                           { self class })
	                          | (universalEnvironment forPackages: {  }).
	andEnvironment := orEnvironmentWithClass & orEnvironmentWithPackage.

	self
		assert: orEnvironmentWithPackage classNames asSet
		equals: self packageClassNames.
	self
		assert: orEnvironmentWithClass classNames asArray
		equals: { self class name }.
	self
		assert: andEnvironment classNames asArray asArray
		equals: { self class name }.

	[ "Now we install a new class in the package"
	newClass := Object classInstaller make: [ :builder |
		            builder
			            superclass: Object;
			            name: #RBBrowserTemporaryClassForTest;
			            package: self class package ].
	"Check that the new class belongs in the or environment with a package"
	self assert: orEnvironmentWithPackage classNames asSet equals: (self packageClassNames copyWith: #RBBrowserTemporaryClassForTest).
	"Check that the new class does not belong in the and scope"
	self deny: (andEnvironment classNames includes: newClass name) ]
		ensure: [ newClass removeFromSystem ]
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testAndEnvironment [
	| originalEnvironment objectPrintStringEnvironment andEnvironment |
	originalEnvironment := universalEnvironment referencesTo: #printOn:.
	objectPrintStringEnvironment := universalEnvironment forClass: Object
				selectors: #(#fullPrintString).
	andEnvironment := objectPrintStringEnvironment & originalEnvironment.
	self universalTestFor: andEnvironment.
	self assert: andEnvironment numberSelectors equals: 1.
	self assert: andEnvironment classNames asArray equals: #(#Object).
	self assert: (andEnvironment protocolsFor: Object) size equals: 1.
	self assert: (objectPrintStringEnvironment & objectPrintStringEnvironment) packages equals: objectPrintStringEnvironment packages.
	andEnvironment := originalEnvironment
				& (universalEnvironment referencesTo: #printString).
	self assert: andEnvironment numberSelectors
				equals: (originalEnvironment referencesTo: #printString) numberSelectors.
	self assert: andEnvironment classNames asSortedCollection
				equals: (originalEnvironment referencesTo: #printString) classNames
						asSortedCollection
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testAndEnvironmentWithStoreOn [
	| originalEnvironment objectPrintStringEnvironment andEnvironment |
	originalEnvironment := universalEnvironment referencesTo: #storeOn:.
	objectPrintStringEnvironment := universalEnvironment forClass: Object
				selectors: #(#storeString).
	andEnvironment := objectPrintStringEnvironment & originalEnvironment.
	self universalTestFor: andEnvironment.
	self assert: andEnvironment numberSelectors equals: 1.
	self assert: andEnvironment classNames asArray equals: #(#Object).
	self assert: (andEnvironment protocolsFor: Object) size equals: 1.
	self assert: (objectPrintStringEnvironment & objectPrintStringEnvironment) packages equals: objectPrintStringEnvironment packages.
	andEnvironment := originalEnvironment
				& (universalEnvironment referencesTo: #printString).
	self assert: andEnvironment numberSelectors
				equals: (originalEnvironment referencesTo: #printString) numberSelectors.
	self assert: andEnvironment classNames asSortedCollection
				equals: (originalEnvironment referencesTo: #printString) classNames
						asSortedCollection
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testAndPackageEnvironmentAndClassEnvironment [

	| packageEnvironment classEnvironment andEnvironment newClass |
	universalEnvironment := RBBrowserEnvironment new.
	packageEnvironment := universalEnvironment forPackages:
		                      { self class package }.
	classEnvironment := universalEnvironment forClasses: { self class }.
	andEnvironment := packageEnvironment & classEnvironment.

	self
		assert: packageEnvironment classNames asSet
		equals: self packageClassNames.
	self
		assert: classEnvironment classNames asArray
		equals: { self class name }.
	self
		assert: andEnvironment classNames asArray asArray
		equals: { self class name }.

	[ "Now we install a new class in the package"
	newClass := Object classInstaller make: [ :builder |
		            builder
			            superclass: Object;
			            name: #RBBrowserTemporaryClassForTest;
			            package: self class package ].

	"Check that the new class does not belong in the scope"
	self deny: (andEnvironment classNames includes: newClass name) ]
		ensure: [ newClass removeFromSystem ]
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testBrowserEnvironment [
	"This test checks the whole image. The other ones use a smaller universe to be faster."

	testingEnvironment := self class environment.
	universalEnvironment := RBBrowserEnvironment new.
	self universalTestFor: universalEnvironment
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testClassEnvironment [
	| aClassEnvironment |
	
	aClassEnvironment := RBClassEnvironment
		onEnvironment: universalEnvironment
		classes: (Array with: Object with: Object class).
	
	self universalTestFor: aClassEnvironment.
	
	self deny: universalEnvironment isClassEnvironment.
	self assert: aClassEnvironment isClassEnvironment.
	self assert: (aClassEnvironment packages size) equals: 1.
	self assert: (aClassEnvironment implementorsOf: #printString) numberSelectors equals: 1.

	self 
		assert: (RBClassEnvironment class: Class) selectors size
    	  	equals: Class methods size.
	self 
		assert: (RBClassEnvironment class: RBRefactoringTestTrait) selectors size
    	  	equals: RBRefactoringTestTrait methods size
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testConstructedClassEnvironment [
	| environment |
	environment := RBClassEnvironment new.
	environment
		addClass: Object;
		addClass: OrderedCollection;
		addClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment includesClass: OrderedCollection).
	self assert: (environment includesClass: Collection).
	self deny: (environment includesClass: Object class).
	environment removeClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment includesClass: OrderedCollection).
	self deny: (environment includesClass: Collection).
	self assert: environment numberClasses equals: 2.
	environment addClass: Object class.
	self assert: environment numberClasses equals: 2.
	self assert: (environment includesClass: Object class).
	environment removeClass: self class.
	self assert: environment numberClasses equals: 2
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testConstructedSelectorEnvironment [
	| environment newEnvironment |
	environment := RBSelectorEnvironment new.
	environment
		addClass: Object selector: #printString;
		addClass: OrderedCollection selector: #add:;
		addClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment selectorsForClass: Object) size equals: 1.
	self assert: (environment includesClass: OrderedCollection).
	self assert: (environment selectorsForClass: OrderedCollection) size equals: 1.
	self assert: (environment includesClass: Collection).
	self assert: (environment selectorsForClass: Collection) size
				equals: Collection selectors size.
	self deny: (environment includesClass: Object class).
	newEnvironment := environment copy.
	newEnvironment
		removeClass: OrderedCollection;
		addClass: Object selector: #printOn:;
		removeClass: Object selector: #printString.
	self assert: (newEnvironment includesClass: Object).
	self deny: (newEnvironment includesSelector: #printString in: Object).
	self deny: (newEnvironment includesClass: OrderedCollection).
	self assert: (newEnvironment includesClass: Collection).
	self assert: newEnvironment numberClasses equals: 2.
	self
		assert: newEnvironment numberSelectors + 1 equals: environment numberSelectors.
	newEnvironment addClass: Object class.
	self assert: newEnvironment numberClasses equals: 2.
	self assert: (newEnvironment includesClass: Object class).
	newEnvironment removeClass: self class.
	self assert: newEnvironment numberClasses equals: 2
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testEnvironmentWrapper [
	| printString wrapper |
	printString := universalEnvironment referencesTo: #printString.
	wrapper := RBBrowserEnvironmentWrapper onEnvironment: printString.
	self assert: wrapper numberSelectors equals: printString numberSelectors.
	self assert: wrapper numberClasses equals: printString numberClasses.
	self assert: wrapper environment identicalTo: printString
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testHierarchyEnvironment [

	| aHierarchyEnvironment |
	aHierarchyEnvironment := RBClassHierarchyEnvironment onEnvironment: universalEnvironment class: self class.
	self universalTestFor: aHierarchyEnvironment.
	self assert: (aHierarchyEnvironment implementorsOf: #testPackageEnvironment) numberSelectors equals: 1.
	self assert: (aHierarchyEnvironment implementorsOf: #setUp) numberSelectors equals:4 
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testImplementorsMatching [
	| print |
	print := universalEnvironment implementorsMatching: '*print*'.
	self universalTestFor: print.
	self assert: (print implementorsOf: #printString) numberSelectors
				equals: (universalEnvironment implementorsOf: #printString) numberSelectors.
	print
		classesAndSelectorsDo: [:class :sel | self assert: ('*print*' match: sel)]
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testMatches [
	| envEnvironment environmentEnvironment |
	envEnvironment := universalEnvironment matches: '*env*'.
	environmentEnvironment := universalEnvironment
				referencesTo: #environment.
	self assert: (envEnvironment referencesTo: #environment) numberSelectors
				equals: environmentEnvironment numberSelectors
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testNotEnvironment [
	| notPrintStringEnvironment printStringEnvironment kernelEnvironment notKernelEnvironment |
	printStringEnvironment := universalEnvironment referencesTo: #printString.
	notPrintStringEnvironment := printStringEnvironment not.
	kernelEnvironment := RBPackageEnvironment packageName: 'Kernel'.
	notKernelEnvironment := kernelEnvironment not.

	self universalTestFor: notPrintStringEnvironment.
	self assertEmpty: (notPrintStringEnvironment referencesTo: #printString).
	self assert: (notPrintStringEnvironment not includesClass: RBBrowserEnvironmentTest).
	self assert: (notPrintStringEnvironment not includesSelector: #testNotEnvironment in: RBBrowserEnvironmentTest).
	self deny: (notKernelEnvironment includesPackage: 'Kernel' asPackage).
	self assertEmpty: (notKernelEnvironment & kernelEnvironment) packages
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testNotEnvironmentWithClassEnvironments [
	| numberEnvironment notNumberEnvironment |
	numberEnvironment := universalEnvironment forClasses: Number withAllSubclasses.
	notNumberEnvironment := numberEnvironment not.

	self universalTestFor: notNumberEnvironment.
	self deny: (notNumberEnvironment includesClass: Number).
	self assertEmpty: (notNumberEnvironment & numberEnvironment) classes.
	self assert: (notNumberEnvironment not includesClass: Number)
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testOrEnvironment [
	| env1 env2 orEnvironment |
	env2 := #printOn:.
	env2 := universalEnvironment forClass: Object selectors: #(#fullPrintString).
	env1 := universalEnvironment forClass: Object selectors: #(#printOn:).
	self assert: (env1 | env2) numberSelectors equals: 2.
	self assert: (env2 | env1) numberSelectors equals: 2.
	self universalTestFor: env1 | env2.
	self assert: (env1 | env1) numberSelectors equals: 1.
	orEnvironment := env1 | env1 not.
	self assert: (env2 | env1) packages size equals: 1.
	self universalTestFor: orEnvironment.
	self assert: orEnvironment numberSelectors equals: universalEnvironment numberSelectors.
	self assert: orEnvironment classNames asSortedCollection equals: universalEnvironment classNames asSortedCollection.
	self
		assert: (orEnvironment protocolsFor: Object)
		equals: ((universalEnvironment protocolsFor: Object) reject: [ :protocolName | (Object allSelectorsInProtocol: protocolName) isEmpty ])
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testPackageEnvironment [

	| aPackageEnvironment |
	aPackageEnvironment := universalEnvironment forPackageNames: (Array with: 'Refactoring-Environment-Tests' with: 'Refactoring-Critics-Tests').
	self universalTestFor: aPackageEnvironment.
	self assert: (aPackageEnvironment implementorsOf: #testPackageEnvironment) numberSelectors equals: 1
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testPragmaEnvironment [
	| aPragmaEnvironment |
	aPragmaEnvironment := universalEnvironment forPragmas: #(primitive:).
	self universalTestFor: aPragmaEnvironment
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testProtocolEnvironment [
	| aProtocolEnvironment size |
	aProtocolEnvironment := RBProtocolEnvironment
				onEnvironment: universalEnvironment
				class: Object
				protocols: #(#printing #testing).
	self universalTestFor: aProtocolEnvironment.
	self
		assert: (aProtocolEnvironment implementorsOf: #printString) numberSelectors
				equals: 1.
	size := 0.
	aProtocolEnvironment classesDo: [:each | size := size + 1].
	self assert: size equals: 1.
	aProtocolEnvironment selectorsForClass: Object
		do:
			[:each |
			self assert: (#(#printing #testing)
						includes: (universalEnvironment whichProtocolIncludes: each in: Object))]
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testRemoveSelectorByAndAndNot [
	| aBrowserEnvironment objectPrintString |
	aBrowserEnvironment := universalEnvironment.
	objectPrintString := RBSelectorEnvironment
				onEnvironment: aBrowserEnvironment.
	objectPrintString addClass: Object selector: #printString.
	self assert: aBrowserEnvironment numberSelectors - 1
				equals: (aBrowserEnvironment & objectPrintString not) numberSelectors.
	self universalTestFor: aBrowserEnvironment & objectPrintString not
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testSelectMethods [
	| environment |
	environment := universalEnvironment selectMethods: [ :each | false ].
	self assert: environment numberSelectors equals: 0.
	self assert: environment numberClasses equals: 0.
	environment := universalEnvironment selectMethods: [ :each | true ].
	self assert: environment numberSelectors equals: universalEnvironment numberSelectors.
	environment := universalEnvironment selectMethods: [ :each | each refersToLiteral: #environment ].
	self assert: environment numberSelectors equals: (universalEnvironment referencesTo: #environment) numberSelectors
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testSelectorEnvironment [
	| printString |
	printString := universalEnvironment referencesTo: #printString.
	self universalTestFor: printString.
	self deny: universalEnvironment isSelectorEnvironment.
	self assert: printString isSelectorEnvironment.
	self assert: printString numberSelectors equals: (printString referencesTo: #printString) numberSelectors.
	self assert: printString numberClasses equals: (printString referencesTo: #printString) numberClasses
]

{ #category : 'tests - environments' }
RBBrowserEnvironmentTest >> testSelectorEnvironmentRemovesEmptyClasses [

	| printString class |
	printString := universalEnvironment referencesTo: #printString.
	class := printString classes anyOne.
	printString selectorsForClass: class do: [ :eachSelector |
		printString removeClass: class selector: eachSelector ].
	self deny: (printString classes includes: class)
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testSystemIntegrity [
	| classes environment |
	classes := IdentitySet new.
	environment := RBBrowserEnvironment new.
	environment allClassesDo: [ :class |
		(classes includes: class)
			ifFalse: [ classes add: class ]
			ifTrue: [ self error: 'The class ' , class name , ' that is available under two or more names. This causes problems with the refactoring browser.' ] ]
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testVariableEnvironmentAddRemove [
	| refs |
	refs := RBVariableEnvironment onEnvironment: universalEnvironment.
	refs addClass: self class instanceVariable: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses equals: 1.
	refs removeClass: self class instanceVariable: 'universalEnvironment'.
	self assert: refs numberSelectors equals: 0.
	self assert: refs numberClasses equals: 0.
	refs addClass: self class instanceVariableReader: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses equals: 1.
	refs removeClass: self class instanceVariableReader: 'universalEnvironment'.
	self assert: refs numberSelectors equals: 0.
	self assert: refs numberClasses equals: 0.
	refs addClass: self class instanceVariableWriter: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses equals: 1.
	refs removeClass: self class instanceVariableWriter: 'universalEnvironment'.
	self assert: refs numberSelectors equals: 0.
	self assert: refs numberClasses equals: 0.
	refs addClass: self class classVariable: 'AClassVariable'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses equals: 1.
	refs removeClass: self class classVariable: 'AClassVariable'.
	self assert: refs numberSelectors equals: 0.
	self assert: refs numberClasses equals: 0
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testVariableEnvironmentClassVars [
	"test variable environment for class vars, refered on the class side only"

	| classRefs |
	classRefs := RBVariableEnvironment on: universalEnvironment referencesToClassVariable: 'ClassSideOnlyVariable' in: self class.
	self universalTestFor: classRefs.
	self denyEmpty: classRefs methods
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testVariableEnvironmentInstVars [
	| refs writers readers classRefs |

	refs := RBVariableEnvironment
		on: universalEnvironment
		referencesToInstanceVariable: 'universalEnvironment'
		in: self class.
	writers := RBVariableEnvironment
		on: universalEnvironment
		writersOfInstanceVariable: 'universalEnvironment'
		in: self class.
	readers := RBVariableEnvironment
		on: universalEnvironment
		readersOfInstanceVariable: 'universalEnvironment'
		in: self class.
	classRefs := RBVariableEnvironment
		on: universalEnvironment
		referencesToClassVariable: 'AClassVariable'
		in: self class.
	self universalTestFor: refs.
	self universalTestFor: writers.
	self universalTestFor: readers.
	self universalTestFor: classRefs.
	self deny: universalEnvironment isVariableEnvironment.
	self assert: refs isVariableEnvironment.
	self assert: refs numberSelectors equals: (writers | readers) numberSelectors
]

{ #category : 'tests' }
RBBrowserEnvironmentTest >> testWithoutClassesRemovesClassesFromSender [
	| environment newEnvironment |
	environment := universalEnvironment forClasses: Number withAllSubclasses.

	newEnvironment := environment withoutClasses: { Number . Integer }.

	self
		assert: newEnvironment classes size
		equals: environment classes size - 2
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> uniqueClassesIn: aBrowserEnvironment [
	| allClasses |
	allClasses := IdentitySet new.
	aBrowserEnvironment classesDo: [ :each |
		self deny: (allClasses includes: each).
		allClasses add: each ]
]

{ #category : 'private' }
RBBrowserEnvironmentTest >> universalTestFor: aBrowserEnvironment [
	self labelFor: aBrowserEnvironment.
	self uniqueClassesIn: aBrowserEnvironment.
	self numberSelectorsFor: aBrowserEnvironment.
	self classNamesFor: aBrowserEnvironment.
	self copyFor: aBrowserEnvironment.
	self packagesFor: aBrowserEnvironment.
	self protocolsFor: aBrowserEnvironment.
	self classesFor: aBrowserEnvironment.
	self keysFor: aBrowserEnvironment.
	self assert: aBrowserEnvironment problemCount = 0 equals: aBrowserEnvironment isEmpty
]

{ #category : 'running' }
RBBrowserEnvironmentTest >> universeClasses [
	"The checks of #universalTestFor: walk their whole universe many times, so I limit it to the classes the tests need instead of using the whole image."

	| testPackageClasses |
	testPackageClasses := #( 'Refactoring-Environment-Tests' 'Refactoring-Critics-Tests' ) flatCollect: [ :name | (self packageOrganizer packageNamed: name) definedClasses ].
	^ testPackageClasses , self class withAllSuperclasses , Number withAllSubclasses , RBBrowserEnvironment withAllSubclasses , {
		  Class.
		  Collection.
		  OrderedCollection }
]
