"
In pharo, the inequality operators are `~=` (for the negation of `=`) and `~~` (for the negation of `==`).
"
Class {
	#name : 'ReNotEqualityOperatorsRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReNotEqualityOperatorsRule class >> group [
	^ 'Bugs'
]

{ #category : 'initialization' }
ReNotEqualityOperatorsRule >> initialize [
	super initialize.
	self
		replace: '``@a != ``@b' with: '``@a ~= ``@b';
		replace: '``@a <> ``@b' with: '``@a ~= ``@b';
		replace: '``@a !== ``@b' with: '``@a ~~ ``@b'
]

{ #category : 'accessing' }
ReNotEqualityOperatorsRule >> name [
	^ 'Use the correct non-equality operators'
]

{ #category : 'accessing' }
ReNotEqualityOperatorsRule >> severity [
	^#error
]
