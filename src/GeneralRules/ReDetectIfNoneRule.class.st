"
Replaces detect:ifNone: and contains: when used with the semantics of anySatisfy: by anySatisfy:.

For example, 

[[[ 
collection detect: [:each | .... ] ifNone: [nil]) notNil	
]]]
is transformed into 

[[[ 
collection anySatisfy: [ :each | .... ])
]]]
"
Class {
	#name : #ReDetectIfNoneRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReDetectIfNoneRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DetectIfNoneRule'
]

{ #category : #accessing }
ReDetectIfNoneRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReDetectIfNoneRule >> initialize [
	super initialize.
	self
		replace: '``@collection contains: [:`each | | `@temps | ``@.Statements]'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) isNil'
		with: '(``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]) not';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) = nil'
		with: '(``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]) not';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) == nil'
		with: '(``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]) not';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) notNil'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~= nil'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]';
		
		replace: '(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~~ nil'
		with: '``@collection anySatisfy: [:`each | | `@temps | ``@.Statements]'
]

{ #category : #accessing }
ReDetectIfNoneRule >> name [
	^ '#detect:ifNone: or #contains: -> #anySatisfy:'
]

{ #category : #accessing }
ReDetectIfNoneRule >> rationale [ 
	^ 'Replaces detect:ifNone: and contains: by anySatisfy:'
]
