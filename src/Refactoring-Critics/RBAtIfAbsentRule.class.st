"
Replaces at:ifAbsent: by at:ifAbsentPut:. Its leads to shorter and more readable code.


"
Class {
	#name : #RBAtIfAbsentRule,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics-TransformationRules
}

{ #category : #accessing }
RBAtIfAbsentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AtIfAbsentRule'
]

{ #category : #accessing }
RBAtIfAbsentRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBAtIfAbsentRule >> initialize [
	super initialize.
	self rewriteRule
		replace: '``@dictionary at: ``@key 
					ifAbsent: [| `@temps | 
							``@.Statements1.
							``@dictionary at: ``@key put: ``@object.
							``@.Statements2.
							``@object]'
		with: '``@dictionary at: ``@key
					ifAbsentPut: [| `@temps |
							``@.Statements1.
							``@.Statements2.
							``@object]';
		replace: '``@dictionary at: ``@key
					ifAbsent: [| `@temps |
							``@.Statements.
							``@dictionary at: ``@key put: ``@object]'
		with: '``@dictionary at: ``@key
					ifAbsentPut: [| `@temps |
							``@.Statements.
							``@object]'
]

{ #category : #accessing }
RBAtIfAbsentRule >> name [
	^ 'at:ifAbsent: -> at:ifAbsentPut:'
]
