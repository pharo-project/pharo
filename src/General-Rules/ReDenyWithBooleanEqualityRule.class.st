"
For a code more optimize and more readable, prefer to use deny: instead of assert:equals: false when you want to verify that something is false.
"
Class {
	#name : 'ReDenyWithBooleanEqualityRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReDenyWithBooleanEqualityRule class >> groupSelector [

	^ #optimizationGroup
]

{ #category : 'accessing' }
ReDenyWithBooleanEqualityRule class >> ruleName [
	^ 'use deny: instead of assert:equals: false'
]

{ #category : 'initialization' }
ReDenyWithBooleanEqualityRule >> initialize [

	super initialize.
	self
		replace: 'self assert: `@expr equals: false'
		with: 'self deny: `@expr';
		replace: 'self assert: `@expr identicalTo: false'
		with: 'self deny: `@expr';
		replace: 'self deny: `@expr equals: false'
		with: 'self assert: `@expr';
		replace: 'self deny: `@expr identicalTo: false' 
		with: 'self assert: `@expr'
]
