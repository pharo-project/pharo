Class {
	#name : 'ReEquivalentSuperclassMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRule [

	self generateMethod: 'myCritiques ^ self myCritiquesOnMethod: self class >> #sampleMethod'.
	self assert: (self myCritiquesOnMethod: self class >> #myCritiques) size equals: 1
]

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRuleNotViolated [

	self generateMethod: 'myCritiques ^true'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #myCritiques)
]

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRuleNotViolatedWithTrait [

	| classFactory superclass class trait |
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new.
	[
		superclass := classFactory make: [ :builder | builder name: 'ASuperclass' ].
		trait := classFactory make: [ :builder |
				         builder
					         beTrait;
					         name: 'ATraitForTests' ].
		class := classFactory make: [ :builder |
				         builder
					         superclass: superclass;
					         name: 'AClassWithATrait';
					         traits: trait ].

		self generateMethod: 'method ^ 1' classified: #accessing in: superclass.
		self generateMethod: 'method ^ 1' classified: #accessing in: trait.
		self assertEmpty: (self myCritiquesOnMethod: class >> #method) ] ensure: [ classFactory cleanUp ]
]
