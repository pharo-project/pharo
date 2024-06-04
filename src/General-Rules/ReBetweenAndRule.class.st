"
Replaces ""a >= b and: [a <= c]"" by ""a between: b and: c.
"
Class {
	#name : 'ReBetweenAndRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReBetweenAndRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReBetweenAndRule class >> ruleName [
	^ '"a >= b and: [a <= c]" -> "a between: b and: c"'
]

{ #category : 'accessing' }
ReBetweenAndRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'BetweenAndRule'
]

{ #category : 'initialization' }
ReBetweenAndRule >> initialize [
	super initialize.
	self
		replace: '``@a >= ``@b and: [``@a <= ``@c]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a >= ``@b & (``@a <= ``@c)' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a and: [``@a <= ``@c]' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a & (``@a <= ``@c)' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c and: [``@a >= ``@b]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c & (``@a >= ``@b)' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a and: [``@a >= ``@b]' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a & (``@a >= ``@b)' with: '``@a between: ``@b and: ``@c';
		replace: '``@a >= ``@b and: [``@c >= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a >= ``@b & (``@c >= ``@a)' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a and: [``@c >= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@b <= ``@a & (``@c >= ``@a)' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c and: [``@b <= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@a <= ``@c & (``@b <= ``@a)' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a and: [``@b <= ``@a]' with: '``@a between: ``@b and: ``@c';
		replace: '``@c >= ``@a & (``@b <= ``@a)' with: '``@a between: ``@b and: ``@c'
]
