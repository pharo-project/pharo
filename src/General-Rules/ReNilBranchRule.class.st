"
this smell arise when a branch nil useless is detected.
"
Class {
	#name : 'ReNilBranchRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReNilBranchRule class >> group [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReNilBranchRule class >> ruleName [
	^ 'branch nil is useless'
]

{ #category : 'initialization' }
ReNilBranchRule >> initialize [

	super initialize
		replace: '`@condition ifTrue: [ `@.statements] ifFalse: [ nil ]'
		with: '`@condition ifTrue: [ `@.statements]';
		replace: '`@condition ifFalse: [ `@.statements] ifTrue: [ nil ]'
		with: '`@condition ifFalse: [ `@.statements]'
]
