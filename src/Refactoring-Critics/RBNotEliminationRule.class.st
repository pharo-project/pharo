"
Eliminate unnecessary not''s in relation of conditionals.
For example test not ifTrue:[] is equivalent to test ifFalse:[]' 

[[[
anObject not ifFalse: block'
]]]

is transformed into

[[[  
anObject ifTrue: block'
]]]

[[[
aCollection select: [ :each | ... anObject not ] 
]]]
	
is transformed into 	

[[[
aCollection reject: [ :each | ... anObject ] 
]]]
"
Class {
	#name : #RBNotEliminationRule,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics-TransformationRules
}

{ #category : #accessing }
RBNotEliminationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'NotEliminationRule'
]

{ #category : #accessing }
RBNotEliminationRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBNotEliminationRule >> initialize [
	super initialize.
	self rewriteRule
		replace: '``@object not not' with: '``@object';
		replace: '``@object isEmpty not' with: '``@object isNotEmpty';
		replace: '``@object not ifTrue: ``@block' with: '``@object ifFalse: ``@block';
		replace: '``@object not ifFalse: ``@block' with: '``@object ifTrue: ``@block';
		replace: '``@collection select: [:`each | | `@temps | ``@.Statements. ``@object not]'
			with: '``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object]';
		replace: '``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object not]'
			with: '``@collection select: [:`each | | `@temps | ``@.Statements. ``@object]';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileTrue: ``@block'
			with: '[| `@temps | ``@.Statements. ``@object] whileFalse: ``@block';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileFalse: ``@block'
			with: '[| `@temps | ``@.Statements. ``@object] whileTrue: ``@block';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileTrue'
			with: '[| `@temps | ``@.Statements. ``@object] whileFalse';
		replace: '[| `@temps | ``@.Statements. ``@object not] whileFalse'
			with: '[| `@temps | ``@.Statements. ``@object] whileTrue';
		replace: '(``@a <= ``@b) not' with: '``@a > ``@b';
		replace: '(``@a < ``@b) not' with: '``@a >= ``@b';
		replace: '(``@a = ``@b) not' with: '``@a ~= ``@b';
		replace: '(``@a == ``@b) not' with: '``@a ~~ ``@b';
		replace: '(``@a ~= ``@b) not' with: '``@a = ``@b';
		replace: '(``@a ~~ ``@b) not' with: '``@a == ``@b';
		replace: '(``@a >= ``@b) not' with: '``@a < ``@b';
		replace: '(``@a > ``@b) not' with: '``@a <= ``@b'
]

{ #category : #accessing }
RBNotEliminationRule >> name [
	^ 'Eliminate unnecessary not''s'
]

{ #category : #accessing }
RBNotEliminationRule >> rationale [  
	^ 'Eliminate unnecessary not''s in relation of conditionals'
]
