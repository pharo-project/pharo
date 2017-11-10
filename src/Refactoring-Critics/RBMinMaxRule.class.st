"
The use of the messages #min: and #max: improves code readability and avoids heavily nested conditionals.

For example 

[[[ 
	a < b ifTrue: [a] ifFalse: [b]
]]]			

is transformed into 

[[[  
a min: b
]]]
			
		
"
Class {
	#name : #RBMinMaxRule,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics-TransformationRules
}

{ #category : #accessing }
RBMinMaxRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MinMaxRule'
]

{ #category : #accessing }
RBMinMaxRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBMinMaxRule >> initialize [
	super initialize.
	self rewriteRule
		replace: '``@a < ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a min: ``@b';
		replace: '``@a <= ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a min: ``@b';
		replace: '``@a > ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a max: ``@b';
		replace: '``@a >= ``@b ifTrue: [``@a] ifFalse: [``@b]' with: '``@a max: ``@b';
		replace: '``@a < ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a max: ``@b';
		replace: '``@a <= ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a max: ``@b';
		replace: '``@a > ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a min: ``@b';
		replace:  '``@a >= ``@b ifTrue: [``@b] ifFalse: [``@a]' with: '``@a min: ``@b';
		replace: '`a < ``@b ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '`a <= ``@b ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '`a < ``@b ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a <= ``@b ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a > ``@b ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a >= ``@b ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '`a > ``@b ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '`a >= ``@b ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b < `a ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '``@b <= `a ifTrue: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '``@b < `a ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b <= `a ifFalse: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b > `a ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b >= `a ifTrue: [`a := ``@b]' with: '`a := `a max: ``@b';
		replace: '``@b > `a ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b';
		replace: '``@b >= `a ifFalse: [`a := ``@b]' with: '`a := `a min: ``@b'
]

{ #category : #accessing }
RBMinMaxRule >> name [
	^ 'Rewrite ifTrue:ifFalse: using min:/max:'
]

{ #category : #accessing }
RBMinMaxRule >> rationale [
	^ 'The use of the messages #min: and #max: improves code readability and avoids heavily nested conditionals.'
]
