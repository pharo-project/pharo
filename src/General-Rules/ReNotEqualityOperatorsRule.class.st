"
In pharo, the inequality operators are `~=` (for the negation of `=`) and `~~` (for the negation of `==`).
"
Class {
	#name : 'ReNotEqualityOperatorsRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
ReNotEqualityOperatorsRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReNotEqualityOperatorsRule class >> ruleName [
	^ 'Use the correct non-equality operators'
]

{ #category : 'accessing' }
ReNotEqualityOperatorsRule class >> severity [
	^#error
]

{ #category : 'initialization' }
ReNotEqualityOperatorsRule >> initialize [
	super initialize.
	self
		replace: '``@a != ``@b' with: '``@a ~= ``@b';
		replace: '``@a <> ``@b' with: '``@a ~= ``@b';
		replace: '``@a !== ``@b' with: '``@a ~~ ``@b'
]
