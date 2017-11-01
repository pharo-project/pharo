"
This smell arises when a class has its class name directly in the source instead of ""self class"". The self class variant allows you to create subclasses without needing to redefine that method.
However we cannot systematically replace Class reference by self class or self because a Class reference is static and a self expression is dynamic. So the programmer may want to send messages to root of an hierarchy and not to the leaf classes. 
"
Class {
	#name : #RBRefersToClassRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBRefersToClassRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBRefersToClassRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'RefersToClassRule'
]

{ #category : #running }
RBRefersToClassRule >> basicCheck: aMethod [
	| class |
	class := aMethod methodClass theNonMetaClass.
	^ aMethod hasLiteral: (class environment associationAt: class name ifAbsent: [ ^ false ])
	
]

{ #category : #running }
RBRefersToClassRule >> checkMethod: aMethod [
	(self basicCheck: aMethod) ifTrue: [ 
		result
			addMethod: aMethod;
			addSearchString: aMethod methodClass name ]
]

{ #category : #accessing }
RBRefersToClassRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBRefersToClassRule >> name [
	^ 'Refers to class name instead of "self class"'
]

{ #category : #accessing }
RBRefersToClassRule >> rationale [
	^ 'Checks for direct reference to classes themselves.'
]
