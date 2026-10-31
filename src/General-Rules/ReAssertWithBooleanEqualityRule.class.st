"
For a code more optimize and more readable, prefer to use assert: instead of assert:equals: true when you want to verify that something is true.
"
Class {
	#name : 'ReAssertWithBooleanEqualityRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReAssertWithBooleanEqualityRule class >> groupSelector [

	^ #optimizationGroup
]

{ #category : 'accessing' }
ReAssertWithBooleanEqualityRule class >> ruleName [
	^ 'use assert: instead of assert:equals: true'
]

{ #category : 'initialization' }
ReAssertWithBooleanEqualityRule >> initialize [

	super initialize.
	self
		replace: 'self assert: `@expr equals: true'
		with: 'self assert: `@expr';
		replace: 'self assert: `@expr identicalTo: true' 
		with: 'self assert: `@expr';
		replace: 'self deny: `@expr equals: true' 
		with: 'self deny: `@expr';
		replace: 'self deny: `@expr identicalTo: true'
		with: 'self deny: `@expr'
]
