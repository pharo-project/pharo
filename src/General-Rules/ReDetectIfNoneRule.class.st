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
	#name : 'ReDetectIfNoneRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReDetectIfNoneRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReDetectIfNoneRule class >> rationale [
	^ 'Replaces detect:ifNone: and contains: by anySatisfy:'
]

{ #category : 'accessing' }
ReDetectIfNoneRule class >> ruleName [
	^ '#detect:ifNone: or #contains: -> #anySatisfy:'
]

{ #category : 'accessing' }
ReDetectIfNoneRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'DetectIfNoneRule'
]

{ #category : 'initialization' }
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
