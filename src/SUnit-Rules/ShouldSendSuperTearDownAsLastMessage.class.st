"
In a test case the tearDown method should call super tearDown as last message
"
Class {
	#name : #ShouldSendSuperTearDownAsLastMessage,
	#superclass : #ReAbstractRule,
	#category : #'SUnit-Rules'
}

{ #category : #'testing-interest' }
ShouldSendSuperTearDownAsLastMessage class >> checksMethod [

	^ true
]

{ #category : #utilities }
ShouldSendSuperTearDownAsLastMessage class >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #utilities }
ShouldSendSuperTearDownAsLastMessage class >> superTearDownNotCalledLastIn: aCompiledMethod [
	"Return true if the method is a tearDown method and a call to super tearDown is not the last message send."

	| searcher |
	searcher := self parseTreeSearcher
		matchesAnyMethodOf:
			#('tearDown             `.@any. super tearDown' 'tearDown | `@temps | `.@any. super tearDown')
			do: [ :node :answer | true ];
		yourself.
	^ (searcher
		executeTree: aCompiledMethod parseTree
		initialAnswer: false) not
]

{ #category : #running }
ShouldSendSuperTearDownAsLastMessage >> basicCheck: aMethod [
	^ (aMethod methodClass inheritsFrom: TestCase) and: [ aMethod selector = #tearDown and: [ self class superTearDownNotCalledLastIn: aMethod ] ]
]

{ #category : #accessing }
ShouldSendSuperTearDownAsLastMessage >> group [

	^ 'SUnit'
]

{ #category : #accessing }
ShouldSendSuperTearDownAsLastMessage >> name [

	^ 'Provide a call to super tearDown as the last message in the tearDown method'
]
