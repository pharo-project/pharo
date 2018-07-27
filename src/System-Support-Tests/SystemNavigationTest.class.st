"
I am an sunit test for SystemNavigation with the default value for its environment (Smalltalk globals).


"
Class {
	#name : #SystemNavigationTest,
	#superclass : #TestCase,
	#instVars : [
		'classFactory',
		'navigator',
		'oldSystemAnnouncer'
	],
	#category : #'System-Support-Tests'
}

{ #category : #testing }
SystemNavigationTest class >> isUnitTest [
	^false
]

{ #category : #accessing }
SystemNavigationTest >> classFactory [
	^classFactory
]

{ #category : #'setUp-tearDown' }
SystemNavigationTest >> createClassFactory [
	^ ClassFactoryWithOrganization newWithOrganization: self organizationToTest
]

{ #category : #utilities }
SystemNavigationTest >> createMethodNamed: selector realParent: class [

	^ RGMethodDefinition realClass: class selector: selector
]

{ #category : #'setUp-tearDown' }
SystemNavigationTest >> createSystemNavigationToTest [
	^SystemNavigation new
]

{ #category : #accessing }
SystemNavigationTest >> environmentToTest [
	^self systemNavigationToTest environment
	
]

{ #category : #accessing }
SystemNavigationTest >> organizationToTest [
	^self environmentToTest organization
	
]

{ #category : #running }
SystemNavigationTest >> setUp [
	super setUp. 
	
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
	
	navigator := self createSystemNavigationToTest.
	classFactory := self createClassFactory.
]

{ #category : #accessing }
SystemNavigationTest >> systemNavigationToTest [
	^navigator
	
]

{ #category : #running }
SystemNavigationTest >> tearDown [
	
	self classFactory cleanUp. 
	SystemAnnouncer announcer: oldSystemAnnouncer.
	super tearDown.
	
]

{ #category : #testing }
SystemNavigationTest >> testAllExistingProtocolsFor [

	| instSideProtocols classSideProtocols |

	instSideProtocols := self systemNavigationToTest allExistingProtocolsFor: true. 
	classSideProtocols := self systemNavigationToTest allExistingProtocolsFor: false.
	 
	self assert: (instSideProtocols allSatisfy: #isSymbol).	
	self assert: (classSideProtocols allSatisfy: #isSymbol).
	self assert: (instSideProtocols includesAll: (Object protocols)).
	self assert: (classSideProtocols includesAll: (Object class protocols)).
]

{ #category : #testing }
SystemNavigationTest >> testAllReferencesToPool [
	| result |
	
	result := self systemNavigationToTest allReferencesToPool: FooSharedPool.
	self assert: result size = 1.
]

{ #category : #testing }
SystemNavigationTest >> testAllReferencesToPoolWhenNoRefences [
	| result |
	
	result := self systemNavigationToTest allReferencesToPool: UnreferencedSharedPool.
	self assert: result size = 0.
]

{ #category : #testing }
SystemNavigationTest >> testAllSendersOfASelector [
	| senders selector class otherClass callers |
	selector := ('aMethod', 'WithSenders') asSymbol.
	
	class := self classFactory newClass.
	class compileSilently: selector asString, ' ^ self'.
	class compileSilently: 'anotherMethod ^self ', selector asString.
	otherClass := self classFactory newClass.
	otherClass compileSilently: 'yetAnotherMethod ^self ', selector asString.
	senders := self systemNavigationToTest allSendersOf: selector.
	self assert: senders size = 2.
	callers := senders collect: [ :methodRef | methodRef selector].
	self assert: (callers includesAll: #(#anotherMethod #yetAnotherMethod))

]

{ #category : #testing }
SystemNavigationTest >> testAllSendersOfReturnsExistingMethods [
	"Non-regression test for
	http://code.google.com/p/pharo/issues/detail?id=2593 "
	| senders |
	senders := self systemNavigationToTest allSendersOf: #methodsFor:.
	senders
		do: [:aReference | self
				assert: (aReference actualClass canUnderstand: aReference selector)]
]

{ #category : #testing }
SystemNavigationTest >> testIsMessageSentInSystemAnswersMethodReference [
	| classesSendingMessage sentMessageSelector anyOne |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [self classFactory newClassInCategory: #One].
	5 timesRepeat: [self classFactory newClassInCategory: #Two].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2), (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [:class|	
		class compileSilently: 'meth self ', sentMessageSelector].
	anyOne :=(self systemNavigationToTest allSendersOf: sentMessageSelector) anyOne.

	self assert: (anyOne isKindOf: RGMethodDefinition) 
]

{ #category : #testing }
SystemNavigationTest >> testIsMessageSentInSystemWithClassesActuallySendngTheMessage [
	| classesSendingMessage sentMessageSelector|
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [self classFactory newClassInCategory: #One].
	5 timesRepeat: [self classFactory newClassInCategory: #Two].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2), (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [:class|	
		class compileSilently: 'meth self ', sentMessageSelector].
	self assert: (self systemNavigationToTest allSendersOf: sentMessageSelector) size = 5
]

{ #category : #testing }
SystemNavigationTest >> testIsMessageSentInSystemWithTheSelectorInsideAnArray [
	| classesSendingMessage sentMessageSelector |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [self classFactory newClassInCategory: #One].
	5 timesRepeat: [self classFactory newClassInCategory: #Two].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2), (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [:class|	
		class compileSilently: 'meth ^#(a b ', sentMessageSelector, ' c)'].
	self assert: (self systemNavigationToTest allSendersOf: sentMessageSelector) size = 5
]

{ #category : #testing }
SystemNavigationTest >> testIsUnsentMessage [
	| class |
	class := self classFactory newClass.
	class compileSilently: 'messageNeverSentInTheSystemXXXXThisIsForTest ^self'.
	self assert: (self systemNavigationToTest isUnsentMessage: class selectors anyOne)
]

{ #category : #testing }
SystemNavigationTest >> testRetrievingCategoriesInAPackage [
	| retrievedCategories |
	
	#(#CategoryForTest #'CategoryForTest-Core' #'CategoryForTest-Tests' #CategoryForTest #ForTestCategory #'ForTestCategory-Core') do: [ :catName | 
		self classFactory newClassInCategory: catName].
	
	retrievedCategories := self systemNavigationToTest categoriesInPackageNamed: 'CategoryForTest'.
	self
		assert: (retrievedCategories includesAll: #(#CategoryForTest #'CategoryForTest-Core' #'CategoryForTest-Tests' )).
	retrievedCategories := self systemNavigationToTest categoriesInPackageNamed: 'CategoryForTest-Core'.
	self
		assert: (retrievedCategories includesAll: #(#'CategoryForTest-Core')). 
	retrievedCategories := self systemNavigationToTest categoriesInPackageNamed: 'CategoryFor'.
	self assert: retrievedCategories isEmpty.
]
