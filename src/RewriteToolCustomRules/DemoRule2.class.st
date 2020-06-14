"
A custom rewrite rule
"
Class {
	#name : #DemoRule2,
	#superclass : #RBTransformationRule,
	#category : #'RewriteToolCustomRules-Demo'
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
	self rewriteRule
		replace:
			'true not
		ifTrue: `@block1
		ifFalse: `@block2'
		with:
			'false
		ifTrue: `@block1
		ifFalse: `@block2'
]
