Class {
	#name : 'ReSizeCheckRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleNotViolated [

	self generateMethod: 'method: aCol aCol size > 0 ifFalse: [ aCol collect: [ :each | each ] ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleWithIfCollect [

	self generateMethod: 'method: aCol aCol size > 0 ifTrue: [ aCol collect: [ :each | each ] ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleWithIfFalse [

	self generateMethod: 'method: aCol aCol isEmpty ifFalse: [ aCol do: [ :each | each ] ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleWithIfTrue [

	self generateMethod: 'method: aCol aCol notEmpty ifTrue: [ aCol do: [ :each | each ] ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]
