"
In a test case the setUp method should call super setUp as first message
"
Class {
	#name : #ShouldSendSuperSetUpAsFirstMessage,
	#superclass : #ReAbstractRule,
	#category : #'SUnit-Rules'
}

{ #category : #'testing-interest' }
ShouldSendSuperSetUpAsFirstMessage class >> checksMethod [

	^ true
]

{ #category : #utilities }
ShouldSendSuperSetUpAsFirstMessage class >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #utilities }
ShouldSendSuperSetUpAsFirstMessage class >> superSetUpNotCalledFirstIn: aCompiledMethod [
	"Return true if the method is a setUp method and a call to super setUp is not the first message send."

	| searcher |
	searcher := self parseTreeSearcher
		matchesAnyMethodOf:
			#('setUp             super setUp. `.@any' 'setUp | `@temps | super setUp. `.@any')
			do: [ :node :answer | true ];
		yourself.
	^ (searcher
		executeTree: aCompiledMethod parseTree
		initialAnswer: false) not
]

{ #category : #running }
ShouldSendSuperSetUpAsFirstMessage >> basicCheck: aMethod [
	^ (aMethod methodClass inheritsFrom: TestCase) and: [ aMethod selector = #setUp and: [ self class superSetUpNotCalledFirstIn: aMethod ] ]
]

{ #category : #accessing }
ShouldSendSuperSetUpAsFirstMessage >> group [

	^ 'SUnit'
]

{ #category : #accessing }
ShouldSendSuperSetUpAsFirstMessage >> name [

	^ 'Provide a call to super setUp as the first message in the setUp method'
]
