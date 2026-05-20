Class {
	#name : 'FloatReferencesRuleTest',
	#superclass : 'TestCase',
	#category : 'General-Rules-Tests-Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Bugs'
}

{ #category : 'tests' }
FloatReferencesRuleTest >> testBasicCheck [

	self assert: (FloatReferencesRule new basicCheck: (GlobalVariable new
				  name: 'BoxedFloat64';
				  yourself))
]

{ #category : 'tests' }
FloatReferencesRuleTest >> testBasicCheck1 [

	self assert: (FloatReferencesRule new basicCheck: (GlobalVariable new
				  name: 'SmallFloat64';
				  yourself))
]

{ #category : 'tests' }
FloatReferencesRuleTest >> testBasicCheck2 [

	self deny: (FloatReferencesRule new basicCheck: (OCMethodNode new selector: (OCSelectorNode new value: #testMethod; yourself)) )
]
