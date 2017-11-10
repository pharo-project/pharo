Class {
	#name : #RBBrowserEnvironmentTest,
	#superclass : #TestCase,
	#instVars : [
		'universalEnvironment'
	],
	#classVars : [
		'ClassSideOnlyVariable',
		'ClassVariable'
	],
	#category : #Refactoring-Tests-Environment
}

{ #category : #accessing }
RBBrowserEnvironmentTest class >> accessFromClassSideOnlyVariable [
	"testing method. This class variable is only accessed from this class side method"

	^ ClassSideOnlyVariable
]

{ #category : #accessing }
RBBrowserEnvironmentTest class >> packageNamesUnderTest [
	^ #('Refactoring-Environment')
]

{ #category : #private }
RBBrowserEnvironmentTest >> categoriesFor: anEnvironment [ 
	| allCategories |
	allCategories := IdentitySet withAll: universalEnvironment categories.
	allCategories removeAll: anEnvironment categories.
	anEnvironment not categories 
		do: [ :each | allCategories remove: each ifAbsent: [ ] ].
	allCategories 
		do: [ :each | self assert: (universalEnvironment classNamesFor: each) isEmpty ]
]

{ #category : #private }
RBBrowserEnvironmentTest >> classNamesFor: anEnvironment [ 
	| classNames allClassNames |
	classNames := IdentitySet new
		addAll: anEnvironment classNames asSet;
		addAll: anEnvironment not classNames;
		yourself.
	allClassNames := universalEnvironment classNames asSortedCollection.
	self assert: classNames asSortedCollection 	= allClassNames.
	self assert: (anEnvironment & anEnvironment not) classNames isEmpty.
	self assert: (anEnvironment | anEnvironment not) classNames asSortedCollection = allClassNames
]

{ #category : #mockup }
RBBrowserEnvironmentTest >> classVariableReader [
	^ ClassVariable
]

{ #category : #mockup }
RBBrowserEnvironmentTest >> classVariableWriter [
	ClassVariable := nil
]

{ #category : #private }
RBBrowserEnvironmentTest >> classesFor: aBrowserEnvironment [ 
	| allClasses |
	allClasses := aBrowserEnvironment classes asSet.
	allClasses addAll: aBrowserEnvironment not classes.
	RBBrowserEnvironment new allClassesDo: [:each | allClasses remove: each].
	self assert: allClasses isEmpty
]

{ #category : #private }
RBBrowserEnvironmentTest >> copyFor: aBrowserEnvironment [ 
	| newEnvironment |
	newEnvironment := aBrowserEnvironment copy.
	self assert: newEnvironment numberSelectors = aBrowserEnvironment numberSelectors.
	self assert: (newEnvironment not & aBrowserEnvironment) numberSelectors = 0
]

{ #category : #private }
RBBrowserEnvironmentTest >> keysFor: aBrowserEnvironment [ 
	| allKeys |
	allKeys := IdentitySet withAll: aBrowserEnvironment keys.
	allKeys addAll: aBrowserEnvironment not keys.
	allKeys removeAll: Smalltalk keys.
	self assert: allKeys isEmpty
]

{ #category : #private }
RBBrowserEnvironmentTest >> labelFor: aBrowserEnvironment [ 
	self deny: aBrowserEnvironment label isNil.
	self deny: aBrowserEnvironment label isEmpty.
	self deny: aBrowserEnvironment defaultLabel isNil.
	self deny: aBrowserEnvironment defaultLabel isEmpty
]

{ #category : #private }
RBBrowserEnvironmentTest >> numberSelectorsFor: aBrowserEnvironment [
	| numberSelectors numberSelectorsNot |
	numberSelectors := aBrowserEnvironment numberSelectors.
	numberSelectorsNot := aBrowserEnvironment not numberSelectors.
	self assert: numberSelectors + numberSelectorsNot = universalEnvironment numberSelectors.
	self assert: (aBrowserEnvironment & aBrowserEnvironment not) numberSelectors = 0.
	self assert: (universalEnvironment & aBrowserEnvironment) numberSelectors = numberSelectors.
	self assert: (aBrowserEnvironment & universalEnvironment) numberSelectors = numberSelectors
]

{ #category : #private }
RBBrowserEnvironmentTest >> protocolsFor: aBrowserEnvironment [ 
	aBrowserEnvironment classesAndSelectorsDo: [ :class :selector |
		| protocol |
		protocol := aBrowserEnvironment whichProtocolIncludes: selector in: class.
		self assert: (aBrowserEnvironment includesProtocol: protocol in: class) ]
]

{ #category : #running }
RBBrowserEnvironmentTest >> setUp [
	super setUp.
	universalEnvironment := RBBrowserEnvironment new
]

{ #category : #private }
RBBrowserEnvironmentTest >> storeStringFor: aBrowserEnvironment [
	| newEnvironment |
	newEnvironment := Smalltalk compiler evaluate: aBrowserEnvironment storeString.
	self assert: newEnvironment numberSelectors = aBrowserEnvironment numberSelectors.
	self assert: (newEnvironment not & aBrowserEnvironment) numberSelectors = 0
	
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testAndEnvironment [
	| originalEnvironment objectPrintStringEnvironment andEnvironment |
	originalEnvironment := universalEnvironment referencesTo: #printOn:.
	objectPrintStringEnvironment := universalEnvironment forClass: Object
				selectors: #(#fullPrintString).
	andEnvironment := objectPrintStringEnvironment & originalEnvironment.
	self universalTestFor: andEnvironment.
	self assert: andEnvironment numberSelectors = 1.
	self assert: andEnvironment classNames asArray = #(#Object).
	self assert: (andEnvironment protocolsFor: Object) size = 1.
	self assert: (objectPrintStringEnvironment & objectPrintStringEnvironment) packages = objectPrintStringEnvironment packages.
	andEnvironment := originalEnvironment 
				& (universalEnvironment referencesTo: #printString).
	self assert: andEnvironment numberSelectors 
				= (originalEnvironment referencesTo: #printString) numberSelectors.
	self assert: andEnvironment classNames asSortedCollection 
				= (originalEnvironment referencesTo: #printString) classNames 
						asSortedCollection
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testAndEnvironmentWithStoreOn [
	| originalEnvironment objectPrintStringEnvironment andEnvironment |
	originalEnvironment := universalEnvironment referencesTo: #storeOn:.
	objectPrintStringEnvironment := universalEnvironment forClass: Object
				selectors: #(#storeString).
	andEnvironment := objectPrintStringEnvironment & originalEnvironment.
	self universalTestFor: andEnvironment.
	self assert: andEnvironment numberSelectors = 1.
	self assert: andEnvironment classNames asArray = #(#Object).
	self assert: (andEnvironment protocolsFor: Object) size = 1.
	self assert: (objectPrintStringEnvironment & objectPrintStringEnvironment) packages = objectPrintStringEnvironment packages.
	andEnvironment := originalEnvironment 
				& (universalEnvironment referencesTo: #printString).
	self assert: andEnvironment numberSelectors 
				= (originalEnvironment referencesTo: #printString) numberSelectors.
	self assert: andEnvironment classNames asSortedCollection 
				= (originalEnvironment referencesTo: #printString) classNames 
						asSortedCollection
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testBrowserEnvironment [
	self universalTestFor: RBBrowserEnvironment new
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testCategoryEnvironment [
	| aCategoryEnvironment |
	self skip: 'Use RBPackageEnvironment instead.'.
	aCategoryEnvironment := RBCategoryEnvironment 
				onEnvironment: RBBrowserEnvironment new
				categories: #(#'Kernel-Objects').
	self universalTestFor: aCategoryEnvironment.
	self assert: (aCategoryEnvironment implementorsOf: #printString) numberSelectors equals: 1
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testClassEnvironment [
	| aClassEnvironment |
	aClassEnvironment := RBClassEnvironment 
		onEnvironment: universalEnvironment 
		classes: (Array with: Object with: Object class).
	self universalTestFor: aClassEnvironment.
	self deny: (universalEnvironment isClassEnvironment).
	self assert: (aClassEnvironment isClassEnvironment).
	self assert: (aClassEnvironment packages size) equals: 1.
	self assert: (aClassEnvironment implementorsOf: #printString) numberSelectors equals: 1.
	
	self assert: (RBClassEnvironment class: Class) selectors size 
    	  equals: Class methods size.
	self assert: (RBClassEnvironment class: TBehavior) selectors size
    	  equals: TBehavior methods size.
]

{ #category : #testing }
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
	self assert: environment numberClasses = 2.
	environment addClass: Object class.
	self assert: environment numberClasses = 2.
	self assert: (environment includesClass: Object class).
	environment removeClass: self class.
	self assert: environment numberClasses = 2
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testConstructedSelectorEnvironment [
	| environment newEnvironment |
	environment := RBSelectorEnvironment new.
	environment
		addClass: Object selector: #printString;
		addClass: OrderedCollection selector: #add:;
		addClass: Collection.
	self assert: (environment includesClass: Object).
	self assert: (environment selectorsForClass: Object) size = 1.
	self assert: (environment includesClass: OrderedCollection).
	self assert: (environment selectorsForClass: OrderedCollection) size = 1.
	self assert: (environment includesClass: Collection).
	self assert: (environment selectorsForClass: Collection) size 
				= Collection selectors size.
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
	self assert: newEnvironment numberClasses = 2.
	self 
		assert: newEnvironment numberSelectors + 1 = environment numberSelectors.
	newEnvironment addClass: Object class.
	self assert: newEnvironment numberClasses = 2.
	self assert: (newEnvironment includesClass: Object class).
	newEnvironment removeClass: self class.
	self assert: newEnvironment numberClasses = 2
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testEnvironmentWrapper [
	| printString wrapper |
	printString := RBBrowserEnvironment new referencesTo: #printString.
	wrapper := RBBrowserEnvironmentWrapper onEnvironment: printString.
	self assert: wrapper numberSelectors = printString numberSelectors.
	self assert: wrapper numberClasses = printString numberClasses.
	self assert: wrapper environment == printString
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testImplementorsMatching [
	| print |
	print := RBBrowserEnvironment new implementorsMatching: '*print*'.
	self universalTestFor: print.
	self assert: (print implementorsOf: #printString) numberSelectors 
				= (RBBrowserEnvironment new implementorsOf: #printString) numberSelectors.
	print 
		classesAndSelectorsDo: [:class :sel | self assert: ('*print*' match: sel)]
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testMatches [
	| envEnvironment environmentEnvironment |
	envEnvironment := RBBrowserEnvironment new matches: '*env*'.
	environmentEnvironment := RBBrowserEnvironment new 
				referencesTo: #environment.
	self assert: (envEnvironment referencesTo: #environment) numberSelectors 
				= environmentEnvironment numberSelectors
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testNotEnvironment [
	| notPrintStringEnvironment printStringEnvironment kernelEnvironment notKernelEnvironment|
	printStringEnvironment := universalEnvironment referencesTo: #printString.
	notPrintStringEnvironment := printStringEnvironment not.
	kernelEnvironment := (RBPackageEnvironment packageName: 'Kernel').
	notKernelEnvironment := kernelEnvironment not.

	self universalTestFor: notPrintStringEnvironment.
	self assert: (notPrintStringEnvironment referencesTo: #printString) isEmpty.
	self assert: (notPrintStringEnvironment not includesClass: RBBrowserEnvironmentTest).
	self assert: (notPrintStringEnvironment not 
			includesSelector: #testNotEnvironment
			in: RBBrowserEnvironmentTest).
	self assert: (notKernelEnvironment includesPackage: 'Kernel' asPackage) not.
	self assert: (notKernelEnvironment & kernelEnvironment) packages isEmpty
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testNotEnvironmentWithClassEnvironments [
	| numberEnvironment notNumberEnvironment |
	numberEnvironment := (RBBrowserEnvironment new) forClasses: Number withAllSubclasses.
	notNumberEnvironment := numberEnvironment not.

	self universalTestFor: notNumberEnvironment.
	self deny: (notNumberEnvironment includesClass: Number).
	self assert: (notNumberEnvironment & numberEnvironment) classes isEmpty.
	self assert: (notNumberEnvironment not includesClass: Number)
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testOrEnvironment [
	| env1 env2 orEnvironment |
	env2 := #printOn:.
	env2 := universalEnvironment forClass: Object selectors: #(#fullPrintString).
	env1 := universalEnvironment forClass: Object selectors: #(#printOn:).
	self assert: (env1 | env2) numberSelectors = 2.
	self assert: (env2 | env1) numberSelectors = 2.
	self universalTestFor: env1 | env2.
	self assert: (env1 | env1) numberSelectors = 1.
	orEnvironment := env1 | env1 not.
	self assert: (env2 | env1) packages size = 1.
	self universalTestFor: orEnvironment.
	self assert: orEnvironment numberSelectors 
				= universalEnvironment numberSelectors.
	self assert: orEnvironment classNames asSortedCollection 
				= universalEnvironment classNames asSortedCollection.
	self assert: (orEnvironment protocolsFor: Object) 
				= ((universalEnvironment protocolsFor: Object) reject: [:each| (Object allSelectorsInProtocol: each) isEmpty ])

]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testPackageEnvironment [
	| aPackageEnvironment |
	aPackageEnvironment := universalEnvironment
		forPackageNames: (Array with: 'Refactoring-Tests-Environment' with: 'Refactoring-Tests-Critics').
	self universalTestFor: aPackageEnvironment.
	self assert: (aPackageEnvironment implementorsOf: #testPackageEnvironment) numberSelectors equals: 1
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testPragmaEnvironment [
	| aPragmaEnvironment |
	aPragmaEnvironment := universalEnvironment forPragmas: #(primitive:).
	self universalTestFor: aPragmaEnvironment
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testProtocolEnvironment [
	| aProtocolEnvironment size |
	aProtocolEnvironment := RBProtocolEnvironment 
				onEnvironment: RBBrowserEnvironment new
				class: Object
				protocols: #(#printing #testing).
	self universalTestFor: aProtocolEnvironment.
	self 
		assert: (aProtocolEnvironment implementorsOf: #printString) numberSelectors 
				= 1.
	size := 0.
	aProtocolEnvironment classesDo: [:each | size := size + 1].
	self assert: size = 1.
	aProtocolEnvironment selectorsForClass: Object
		do: 
			[:each | 
			self assert: (#(#printing #testing) 
						includes: (RBBrowserEnvironment new whichProtocolIncludes: each in: Object))]
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testRemoveSelectorByAndAndNot [
	| aBrowserEnvironment objectPrintString |
	aBrowserEnvironment := RBBrowserEnvironment new.
	objectPrintString := RBSelectorEnvironment 
				onEnvironment: aBrowserEnvironment.
	objectPrintString addClass: Object selector: #printString.
	self assert: aBrowserEnvironment numberSelectors - 1 
				= (aBrowserEnvironment & objectPrintString not) numberSelectors.
	self universalTestFor: aBrowserEnvironment & objectPrintString not
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testSelectMethods [
	| environment |
	environment := RBBrowserEnvironment new selectMethods: [:each | false].
	self assert: environment numberSelectors = 0.
	self assert: environment numberClasses = 0.
	environment := RBBrowserEnvironment new selectMethods: [:each | true].
	self assert: environment numberSelectors 
				= RBBrowserEnvironment new numberSelectors.
	environment := RBBrowserEnvironment new 
				selectMethods: [:each | each refersToLiteral: #environment].
	self assert: environment numberSelectors 
				= (RBBrowserEnvironment new referencesTo: #environment) numberSelectors
]

{ #category : #testing-environments }
RBBrowserEnvironmentTest >> testSelectorEnvironment [
	| printString |
	printString := RBBrowserEnvironment new referencesTo: #printString.
	self universalTestFor: printString.
	self deny: universalEnvironment isSelectorEnvironment.
	self assert: printString isSelectorEnvironment.
	self assert: printString numberSelectors = (printString referencesTo: #printString) numberSelectors.
	self assert: printString numberClasses = (printString referencesTo: #printString) numberClasses
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testSystemIntegrity [
	| classes environment |
	classes := IdentitySet new.
	environment := RBBrowserEnvironment new.
	environment allClassesDo: [ :class |
		(classes includes: class)
			ifFalse: [ classes add: class ]
			ifTrue: [ self error: 'The class ' , class name , ' that is available under two or more names. This causes problems with the refactoring browser.' ] ]
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testVariableEnvironmentAddRemove [
	| refs |
	refs := RBVariableEnvironment new.
	refs addClass: self class instanceVariable: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class instanceVariable: 'universalEnvironment'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0.
	refs addClass: self class instanceVariableReader: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class instanceVariableReader: 'universalEnvironment'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0.
	refs addClass: self class instanceVariableWriter: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class instanceVariableWriter: 'universalEnvironment'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0.
	refs addClass: self class classVariable: 'ClassVariable'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class classVariable: 'ClassVariable'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0
	
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testVariableEnvironmentClassVars [
"test variable environment for class vars, refered on the class side only"
	| classRefs |
	classRefs := RBVariableEnvironment
		referencesToClassVariable: 'ClassSideOnlyVariable'
		in: self class.
	self universalTestFor: classRefs.
	self assert: classRefs methods isEmpty not.

]

{ #category : #testing }
RBBrowserEnvironmentTest >> testVariableEnvironmentInstVars [
	| refs writers readers classRefs |
	refs := RBVariableEnvironment
		referencesToInstanceVariable: 'universalEnvironment'
		in: self class.
	writers := RBVariableEnvironment
		writersOfInstanceVariable: 'universalEnvironment'
		in: self class.
	readers := RBVariableEnvironment
		readersOfInstanceVariable: 'universalEnvironment'
		in: self class.
	classRefs := RBVariableEnvironment
		referencesToClassVariable: 'ClassVariable'
		in: self class.
	self universalTestFor: refs.
	self universalTestFor: writers.
	self universalTestFor: readers.
	self universalTestFor: classRefs.
	self deny: universalEnvironment isVariableEnvironment.
	self assert: refs isVariableEnvironment.
	self assert: refs numberSelectors = (writers | readers) numberSelectors
]

{ #category : #testing }
RBBrowserEnvironmentTest >> testWithoutClassesRemovesClassesFromSender [
	| environment newEnvironment |
	environment := (RBBrowserEnvironment new) forClasses: Number withAllSubclasses.
	
	newEnvironment := environment withoutClasses: { Number . Integer }.
	
	self 
		assert: newEnvironment classes size
		equals: environment classes size - 2
]

{ #category : #private }
RBBrowserEnvironmentTest >> uniqueClassesIn: aBrowserEnvironment [ 
	| allClasses |
	allClasses := IdentitySet new.
	aBrowserEnvironment classesDo: [ :each | 
		self deny: (allClasses includes: each).
		allClasses add: each ]
]

{ #category : #private }
RBBrowserEnvironmentTest >> universalTestFor: aBrowserEnvironment [ 
	self labelFor: aBrowserEnvironment.
	self uniqueClassesIn: aBrowserEnvironment.
	self numberSelectorsFor: aBrowserEnvironment.
	self storeStringFor: aBrowserEnvironment.
	self classNamesFor: aBrowserEnvironment.
	self copyFor: aBrowserEnvironment.
	self categoriesFor: aBrowserEnvironment.
	self protocolsFor: aBrowserEnvironment.
	self classesFor: aBrowserEnvironment.
	self keysFor: aBrowserEnvironment.
	self assert: aBrowserEnvironment problemCount = 0 = aBrowserEnvironment isEmpty
]
