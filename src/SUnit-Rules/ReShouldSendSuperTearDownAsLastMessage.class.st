"
In a test case the tearDown method should call super tearDown as last message
"
Class {
	#name : 'ReShouldSendSuperTearDownAsLastMessage',
	#superclass : 'ReAbstractRule',
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : 'SUnit-Rules-SUnit',
	#package : 'SUnit-Rules',
	#tag : 'SUnit'
}

{ #category : 'testing - interest' }
ReShouldSendSuperTearDownAsLastMessage class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReShouldSendSuperTearDownAsLastMessage class >> group [
	^ self sunitGroup
]

{ #category : 'utilities' }
ReShouldSendSuperTearDownAsLastMessage class >> parseTreeSearcher [

	^ OCParseTreeSearcher new
]

{ #category : 'accessing' }
ReShouldSendSuperTearDownAsLastMessage class >> ruleName [
	^ 'Provide a call to super tearDown as the last message in the tearDown method'
]

{ #category : 'utilities' }
ReShouldSendSuperTearDownAsLastMessage class >> superTearDownNotCalledLastIn: aCompiledMethod [
	"Return true if the method is a tearDown method and a call to super tearDown is not the last message send."

	| searcher |
	searcher := self parseTreeSearcher
		matchesAnyMethodOf:
			#('tearDown             `.@any. super tearDown' 'tearDown | `@temps | `.@any. super tearDown')
			do: [ :node :answer | true ];
		yourself.
	^ (searcher
		executeTree: aCompiledMethod ast
		initialAnswer: false) not
]

{ #category : 'running' }
ReShouldSendSuperTearDownAsLastMessage >> basicCheck: aMethod [

	^ (self isClassToCheck: aMethod methodClass) and: [ aMethod selector = #tearDown and: [ self class superTearDownNotCalledLastIn: aMethod ] ]
]

{ #category : 'testing' }
ReShouldSendSuperTearDownAsLastMessage >> isClassToCheck: aClass [

	^ { TestCase. TestResource } anySatisfy: [:each | aClass inheritsFrom: each ]
]
