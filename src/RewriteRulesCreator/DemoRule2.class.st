"
I am a demo rewrite rule.
"
Class {
	#name : #DemoRule2,
	#superclass : #RBCustomTransformationRule,
	#category : #'RewriteRulesCreator-Demo'
}

{ #category : #accessing }
DemoRule2 >> fooMethod [
	true not
		ifTrue: [ ^ 1 + 1 ]
		ifFalse: [ ^ 1 + 0 ]
]

{ #category : #initialization }
DemoRule2 >> initialize [
	super initialize.
	searchFor := 'true not
		ifTrue: `@block1
		ifFalse: `@block2'.
	replaceWith := 'false
		ifTrue: `@block1
		ifFalse: `@block2'.
	input := ''.
	output := ''.
	rewriteRule replace: searchFor with: replaceWith
]
