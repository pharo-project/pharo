"
Using assert:equals: produces better context on rule failure
"
Class {
	#name : #AssertEqualSignIntoAssertEquals,
	#superclass : #ReNodeRewriteRule,
	#category : #'SUnit-Rules'
}

{ #category : #accessing }
AssertEqualSignIntoAssertEquals class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AssertEqualSignIntoAssertEquals'
]

{ #category : #hooks }
AssertEqualSignIntoAssertEquals >> afterCheck: aNode mappings: mappingDict [
	"would be nice to test if the class uses TAssertable,
	 but it is more complicated"
	
	^ [ aNode methodNode methodClass canUnderstand: #assert:equals: ]
		on: MessageNotUnderstood "methodNode or methodClass may be nil"
		do: [ false ]
]

{ #category : #accessing }
AssertEqualSignIntoAssertEquals >> group [

	^ 'SUnit'
]

{ #category : #initialization }
AssertEqualSignIntoAssertEquals >> initialize [
	super initialize.
	self replace: 'self assert: ``@object1 = ``@object2' with: 'self assert: ``@object1 equals: ``@object2'.
	self replace: 'self deny: ``@object1 = ``@object2' with: 'self deny: ``@object1 equals: ``@object2'.
	self replace: 'self assert: ``@object1 == ``@object2' with: 'self assert: ``@object1 identicalTo: ``@object2'.
	self replace: 'self deny: ``@object1 == ``@object2' with: 'self deny: ``@object1 identicalTo: ``@object2'
]

{ #category : #accessing }
AssertEqualSignIntoAssertEquals >> name [

	^ 'Use assert:equals: instead of assert: and = (or deny:/identicalTo: and ==)'
]
