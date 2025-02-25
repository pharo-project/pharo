"
In a test case the setUp method should call super setUp as first message
"
Class {
	#name : 'ReShouldSendSuperSetUpAsFirstMessage',
	#superclass : 'ReAbstractRule',
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : 'SUnit-Rules-SUnit',
	#package : 'SUnit-Rules',
	#tag : 'SUnit'
}

{ #category : 'testing - interest' }
ReShouldSendSuperSetUpAsFirstMessage class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReShouldSendSuperSetUpAsFirstMessage class >> group [
	^ self sunitGroup
]

{ #category : 'utilities' }
ReShouldSendSuperSetUpAsFirstMessage class >> parseTreeSearcher [

	^ OCParseTreeSearcher new
]

{ #category : 'accessing' }
ReShouldSendSuperSetUpAsFirstMessage class >> ruleName [
	^ 'Provide a call to super setUp as the first message in the setUp method'
]

{ #category : 'utilities' }
ReShouldSendSuperSetUpAsFirstMessage class >> superSetUpNotCalledFirstIn: aCompiledMethod [
	"Return true if the method is a setUp method and a call to super setUp is not the first message send."

	| searcher |
	searcher := self parseTreeSearcher
		matchesAnyMethodOf:
			#('setUp             super setUp. `.@any' 'setUp | `@temps | super setUp. `.@any')
			do: [ :node :answer | true ];
		yourself.
	^ (searcher
		executeTree: aCompiledMethod ast
		initialAnswer: false) not
]

{ #category : 'running' }
ReShouldSendSuperSetUpAsFirstMessage >> basicCheck: aMethod [

	^ (self isClassToCheck: aMethod methodClass) and: [ aMethod selector = #setUp and: [ self class superSetUpNotCalledFirstIn: aMethod ] ]
]

{ #category : 'testing' }
ReShouldSendSuperSetUpAsFirstMessage >> isClassToCheck: aClass [

	^ { TestCase. TestResource } anySatisfy: [:each | aClass inheritsFrom: each ]
]
