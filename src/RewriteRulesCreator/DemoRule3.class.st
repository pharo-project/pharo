"
I am a demo rewrite rule.
"
Class {
	#name : #DemoRule3,
	#superclass : #RBCustomTransformationRule,
	#category : #'RewriteRulesCreator-Demo'
}

{ #category : #initialization }
DemoRule3 >> initialize [
	super initialize.
	searchFor := 'true not
		ifTrue: [ ^ 1 + 1 ]
		ifFalse: [ ^ 1 + 0 ]'.
	replaceWith := 'false
		ifTrue: [ ^ 1 + 1 ]
		ifFalse: [ ^ 1 + 0 ]'.
	input := ''.
	output := ''.
	rewriteRule replace: searchFor with: replaceWith
]
