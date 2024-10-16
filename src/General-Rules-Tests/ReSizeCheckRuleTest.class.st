Class {
	#name : 'ReSizeCheckRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleNotViolated [ 

	| critiques | 
	
	self class compile: 'method: aCol aCol size > 0 ifFalse: [ aCol collect: [ :each | each ] ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method:) removeFromSystem ]
	 
]

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleWithIfCollect [

	| critiques | 
	
	self class compile: 'method: aCol aCol size > 0 ifTrue: [ aCol collect: [ :each | each ] ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
	 
]

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleWithIfFalse [ 

	| critiques | 
	
	self class compile: 'method: aCol aCol isEmpty ifFalse: [ aCol do: [ :each | each ] ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
	 
]

{ #category : 'tests' }
ReSizeCheckRuleTest >> testRuleWithIfTrue [

	| critiques | 
	
	self class compile: 'method: aCol aCol notEmpty ifTrue: [ aCol do: [ :each | each ] ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
	 
]
