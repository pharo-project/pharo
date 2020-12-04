"
Using assert:equals: produces better context on rule failure
"
Class {
	#name : #ReAssertEqualSignIntoAssertEqualsRule,
	#superclass : #ReNodeRewriteRule,
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : #'SUnit-Rules-Base'
}

{ #category : #accessing }
ReAssertEqualSignIntoAssertEqualsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AssertEqualSignIntoAssertEquals'
]

{ #category : #hooks }
ReAssertEqualSignIntoAssertEqualsRule >> afterCheck: aNode mappings: mappingDict [
	"would be nice to test if the class uses TAssertable,
	 but it is more complicated"
	
	^ [ aNode methodNode methodClass canUnderstand: #assert:equals: ]
		on: MessageNotUnderstood "methodNode or methodClass may be nil"
		do: [ false ]
]

{ #category : #initialization }
ReAssertEqualSignIntoAssertEqualsRule >> initialize [
	super initialize.
	self replace: 'self assert: ``@object1 = ``@object2' with: 'self assert: ``@object1 equals: ``@object2'.
	self replace: 'self deny: ``@object1 = ``@object2' with: 'self deny: ``@object1 equals: ``@object2'.
	self replace: 'self assert: ``@object1 == ``@object2' with: 'self assert: ``@object1 identicalTo: ``@object2'.
	self replace: 'self deny: ``@object1 == ``@object2' with: 'self deny: ``@object1 identicalTo: ``@object2'
]

{ #category : #accessing }
ReAssertEqualSignIntoAssertEqualsRule >> name [

	^ 'Use assert:equals: instead of assert: and = (or deny:/identicalTo: and ==)'
]
