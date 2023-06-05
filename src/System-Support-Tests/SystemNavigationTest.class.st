"
I am an sunit test for SystemNavigation with the default value for its environment (Smalltalk globals).


"
Class {
	#name : #SystemNavigationTest,
	#superclass : #TestCase,
	#instVars : [
		'classFactory',
		'oldSystemAnnouncer'
	],
	#category : #'System-Support-Tests-Image'
}

{ #category : #testing }
SystemNavigationTest class >> isUnitTest [
	^false
]

{ #category : #accessing }
SystemNavigationTest >> classFactory [

	^classFactory
]

{ #category : #utilities }
SystemNavigationTest >> createClassFactory [

	^ ClassFactoryWithOrganization newWithOrganization: self systemNavigationToTest environment organization
]

{ #category : #running }
SystemNavigationTest >> setUp [

	super setUp.
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
	classFactory := self createClassFactory
]

{ #category : #accessing }
SystemNavigationTest >> systemNavigationToTest [

	^SystemNavigation new
]

{ #category : #running }
SystemNavigationTest >> tearDown [

	self classFactory cleanUp.
	SystemAnnouncer announcer: oldSystemAnnouncer.
	super tearDown
]

{ #category : #tests }
SystemNavigationTest >> testAllClassesImplementing [

	| implementingClasses |
	implementingClasses := self systemNavigationToTest
		allClassesImplementing: #isSymbol.
	self
		assertCollection: implementingClasses
		hasSameElements: {Object. Symbol}
]

{ #category : #tests }
SystemNavigationTest >> testAllExistingProtocolsFor [

	| instSideProtocols classSideProtocols |
	instSideProtocols := self systemNavigationToTest allExistingProtocolsFor: true.
	classSideProtocols := self systemNavigationToTest allExistingProtocolsFor: false.

	self assert: (instSideProtocols allSatisfy: #isSymbol).
	self assert: (classSideProtocols allSatisfy: #isSymbol).
	self assert: (instSideProtocols includesAll: Object protocolNames).
	self assert: (classSideProtocols includesAll: Object class protocolNames)
]

{ #category : #tests }
SystemNavigationTest >> testAllSendersOfASelector [

	| senders selector class otherClass callers |
	selector := ('aMethod' , 'WithSenders') asSymbol.
	class := self classFactory newClass.
	class compileSilently: selector asString , ' ^ self'.
	class compileSilently: 'anotherMethod ^self ' , selector asString.
	otherClass := self classFactory newClass.
	otherClass
		compileSilently: 'yetAnotherMethod ^self ' , selector asString.
	senders := self systemNavigationToTest allSendersOf: selector.
	self assert: senders size equals: 2.
	callers := senders collect: [ :methodRef | methodRef selector ].
	self
		assert: (callers includesAll: #(#anotherMethod #yetAnotherMethod))
]

{ #category : #tests }
SystemNavigationTest >> testIsMessageSentInSystemWithClassesActuallySendngTheMessage [

	| classesSendingMessage sentMessageSelector |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [ self classFactory newClassInCategory: #One ].
	5 timesRepeat: [ self classFactory newClassInCategory: #Two ].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2) , (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [ :class | class compileSilently: 'meth self ' , sentMessageSelector ].
	self assert: (self systemNavigationToTest allSendersOf: sentMessageSelector) size equals: 5
]

{ #category : #tests }
SystemNavigationTest >> testIsMessageSentInSystemWithTheSelectorInsideAnArray [

	| classesSendingMessage sentMessageSelector |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [ self classFactory newClassInCategory: #One ].
	5 timesRepeat: [ self classFactory newClassInCategory: #Two ].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2) , (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [ :class | class compileSilently: 'meth ^#(a b ' , sentMessageSelector , ' c)' ].
	self assert: (self systemNavigationToTest allSendersOf: sentMessageSelector) size equals: 5
]

{ #category : #tests }
SystemNavigationTest >> testIsUnsentMessage [

	| class |
	class := self classFactory newClass.
	class compileSilently: 'messageNeverSentInTheSystemXXXXThisIsForTest ^self'.
	self assert: (self systemNavigationToTest isUnsentMessage: class selectors anyOne)
]

{ #category : #tests }
SystemNavigationTest >> testReferencesToAClassInBlock [

	| referencingMethods class otherClass callers |

	class := self classFactory newClass.
	class compileSilently: 'aMethod ^ 42'.

	otherClass := self classFactory newClass.
	otherClass
		compileSilently: 'anotherMethod ^[[' , class name asString ,  ']]'.

	referencingMethods := self systemNavigationToTest allReferencesTo: class binding.
	self assert: referencingMethods size equals: 1.
	callers := referencingMethods collect: [ :methodRef | methodRef selector ].
	self
		assert: (callers includesAll: #(#anotherMethod))
]

{ #category : #tests }
SystemNavigationTest >> testSenderOfASelectorInBlock [

	| senders selector class otherClass callers |
	selector := ('aMethod' , 'WithSenders') asSymbol.
	class := self classFactory newClass.
	class compileSilently: selector asString , ' ^ self'.
	class compileSilently: 'anotherMethod ^ [self ' , selector asString , ']'.

	otherClass := self classFactory newClass.
	otherClass
		compileSilently: 'yetAnotherMethod ^[[self ' , selector asString , ']]'.

	senders := self systemNavigationToTest allSendersOf: selector.
	self assert: senders size equals: 2.
	callers := senders collect: [ :methodRef | methodRef selector ].
	self
		assert: (callers includesAll: #(#anotherMethod #yetAnotherMethod))
]
