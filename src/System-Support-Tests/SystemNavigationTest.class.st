"
I am an sunit test for SystemNavigation with the default value for its environment (Smalltalk globals).


"
Class {
	#name : 'SystemNavigationTest',
	#superclass : 'TestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'System-Support-Tests-Image',
	#package : 'System-Support-Tests',
	#tag : 'Image'
}

{ #category : 'accessing' }
SystemNavigationTest >> classFactory [

	^classFactory
]

{ #category : 'utilities' }
SystemNavigationTest >> createClassFactory [

	^ ClassFactoryForTestCase environment: self systemNavigationToTest environment
]

{ #category : 'private' }
SystemNavigationTest >> performTest [

	self class codeChangeAnnouncer suspendAllWhile: [ ^ super performTest ]
]

{ #category : 'running' }
SystemNavigationTest >> setUp [

	super setUp.
	classFactory := self createClassFactory
]

{ #category : 'accessing' }
SystemNavigationTest >> systemNavigationToTest [

	^SystemNavigation new
]

{ #category : 'running' }
SystemNavigationTest >> tearDown [

	self classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
SystemNavigationTest >> testAllClassesImplementing [

	| implementingClasses |
	implementingClasses := self systemNavigationToTest
		allClassesImplementing: #isSymbol.
	self
		assertCollection: implementingClasses
		hasSameElements: {Object. Symbol}
]

{ #category : 'tests' }
SystemNavigationTest >> testAllExistingProtocolsFor [

	| instSideProtocols classSideProtocols |
	instSideProtocols := self systemNavigationToTest allExistingProtocolsFor: true.
	classSideProtocols := self systemNavigationToTest allExistingProtocolsFor: false.

	self assert: (instSideProtocols allSatisfy: #isSymbol).
	self assert: (classSideProtocols allSatisfy: #isSymbol).
	self assert: (instSideProtocols includesAll: Object protocolNames).
	self assert: (classSideProtocols includesAll: Object class protocolNames)
]

{ #category : 'tests' }
SystemNavigationTest >> testAllGlobalNamesStartingWithDoCaseSensitive [

	| result |
	result := OrderedCollection new.
	self systemNavigationToTest allGlobalNamesStartingWith: 'Sma' do: [ :name | result add: name ] caseSensitive: true.
	self assert: (result includes: 'Smalltalk').

	result := OrderedCollection new.
	self systemNavigationToTest allGlobalNamesStartingWith: 'sma' do: [ :name | result add: name ] caseSensitive: true.
	self deny: (result includes: 'Smalltalk').

	result := OrderedCollection new.
	self systemNavigationToTest allGlobalNamesStartingWith: 'sma' do: [ :name | result add: name ] caseSensitive: false.
	self assert: (result includes: 'Smalltalk')
]

{ #category : 'tests' }
SystemNavigationTest >> testAllReferencesToDo [
	"test trying to not hard code numbers as they will be different"
	| counterSymbol counterClass |
	counterSymbol := 0.
	SystemNavigation new allReferencesTo: #Object do: [:ref | counterSymbol := counterSymbol +1 ].
	self assert: counterSymbol > 10.
	counterClass := 0.
	SystemNavigation new allReferencesTo: Object binding do: [:ref | counterClass := counterClass +1 ].
	self assert: counterClass > 10.
	"and there are more refs to the class then to the symbol"
	self assert: counterClass > counterSymbol.
	
]

{ #category : 'tests' }
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

{ #category : 'tests' }
SystemNavigationTest >> testIsMessageSentInSystemWithClassesActuallySendngTheMessage [

	| classesSendingMessage sentMessageSelector |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [ self classFactory make: [ :aBuilder | aBuilder tag: #One ] ].
	5 timesRepeat: [ self classFactory make: [ :aBuilder | aBuilder tag: #Two ] ].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2) , (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [ :class | class compileSilently: 'meth self ' , sentMessageSelector ].
	self
		assertCollection: (self systemNavigationToTest allSendersOf: sentMessageSelector)
		hasSameElements: (classesSendingMessage collect: [ :class | class >> #meth ])
]

{ #category : 'tests' }
SystemNavigationTest >> testIsMessageSentInSystemWithTheSelectorInsideAnArray [

	| classesSendingMessage sentMessageSelector |
	sentMessageSelector := 'MessageSentOnlyByTestClassesXXXShouldNotBeRealyDefined' asSymbol.
	5 timesRepeat: [ self classFactory make: [ :aBuilder | aBuilder tag: #One ] ].
	5 timesRepeat: [ self classFactory make: [ :aBuilder | aBuilder tag: #Two ] ].
	classesSendingMessage := (self classFactory createdClasses asArray first: 2) , (self classFactory createdClasses asArray last: 3).
	classesSendingMessage do: [ :class | class compileSilently: 'meth ^#(a b ' , sentMessageSelector , ' c)' ].
	self
		assertCollection: (self systemNavigationToTest allSendersOf: sentMessageSelector)
		hasSameElements: (classesSendingMessage collect: [ :class | class >> #meth ])
]

{ #category : 'tests' }
SystemNavigationTest >> testIsUnsentMessage [

	| class |
	class := self classFactory newClass.
	class compileSilently: 'messageNeverSentInTheSystemXXXXThisIsForTest ^self'.
	self assert: (self systemNavigationToTest isUnsentMessage: class selectors anyOne)
]

{ #category : 'tests' }
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

{ #category : 'tests' }
SystemNavigationTest >> testRemoveMethodInClass [

	| methodToRemove selector class |

	selector := #aMethod.
	class := self classFactory silentlyNewClass.
	class compileSilently: selector asString , ' ^ self'.

	methodToRemove := class >> selector.
	self assert: (self systemNavigationToTest removeMethod: methodToRemove inClass: class).
	self should: [ class >> selector ] raise: NotFound
]

{ #category : 'tests' }
SystemNavigationTest >> testSenderOfASelectorInBlock [

	| senders selector class otherClass |
	selector := ('aMethod' , 'WithSenders') asSymbol.
	class := self classFactory silentlyNewClass.
	class compileSilently: selector asString , ' ^ self'.
	class compileSilently: 'anotherMethod ^ [self ' , selector asString , ']'.

	otherClass := self classFactory silentlyNewClass.
	otherClass compileSilently: 'yetAnotherMethod ^[[self ' , selector asString , ']]'.

	senders := self systemNavigationToTest allSendersOf: selector.
	self assert: (senders collect: [ :method | method selector ]) asSet equals: { #anotherMethod. #yetAnotherMethod } asSet
]
