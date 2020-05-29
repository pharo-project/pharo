"
Replaces at:ifAbsent: by at:ifAbsentPut:. Its leads to shorter and more readable code.


"
Class {
	#name : #ReAtIfAbsentRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReAtIfAbsentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'AtIfAbsentRule'
]

{ #category : #accessing }
ReAtIfAbsentRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReAtIfAbsentRule >> initialize [
	super initialize.
	self
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
ReAtIfAbsentRule >> name [
	^ 'at:ifAbsent: -> at:ifAbsentPut:'
]
