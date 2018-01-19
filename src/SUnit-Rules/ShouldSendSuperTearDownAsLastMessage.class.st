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

{ #category : #utility }
ShouldSendSuperTearDownAsLastMessage class >> superTearDownNotCalledLastIn: aCompiledMethod [
	"Return true if the method is a tearDown method and a call to super tearDown is not the last message send."
	
	| searcher |
	searcher := RBParseTreeSearcher new 
						matchesAnyMethodOf: #(
							'tearDown             `.@any. super tearDown' 
							'tearDown | `@temps | `.@any. super tearDown') 
						do: [ :node :answer | true ];
						yourself.
	^(searcher executeTree: aCompiledMethod parseTree initialAnswer: false) not
]

{ #category : #running }
ShouldSendSuperTearDownAsLastMessage >> check: aMethod forCritiquesDo: aCritiqueBlock [

	((aMethod methodClass inheritsFrom: TestCase)
		and: [ aMethod selector = #tearDown and: [ self class superTearDownNotCalledLastIn: aMethod ]])
			ifTrue: [ aCritiqueBlock cull: (self critiqueFor: aMethod) ]
]

{ #category : #accessing }
ShouldSendSuperTearDownAsLastMessage >> group [

	^ 'SUnit'
]

{ #category : #accessing }
ShouldSendSuperTearDownAsLastMessage >> name [

	^ 'Provide a call to super tearDown as the last message in the tearDown method'
]
