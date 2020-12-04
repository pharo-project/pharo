"
I am a demo rewrite rule.
"
Class {
	#name : #DemoRule1,
	#superclass : #RBCustomTransformationRule,
	#category : #'RewriteRulesCreator-Demo'
}

{ #category : #initialization }
DemoRule1 >> initialize [
	super initialize.
	searchFor := '`variable1 isNil
	ifTrue: `@block1.
`.Statement1'.
	replaceWith := '`variable1 ifNil: `@block1.
`.Statement1'.
	input := ''.
	output := ''.
	rewriteRule replace: searchFor with: replaceWith
]

{ #category : #accessing }
DemoRule1 >> someMethod [
	self isNil ifTrue: [ ^ true ].
	super size
]
