"
Classes that use traits with explicit requirement methods should either implement the method or inherit it.
"
Class {
	#name : 'ReExplicitRequirementMethodsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'testing' }
ReExplicitRequirementMethodsRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReExplicitRequirementMethodsRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReExplicitRequirementMethodsRule class >> ruleName [
	^ 'Explicit requirement methods'
]

{ #category : 'accessing' }
ReExplicitRequirementMethodsRule class >> uniqueIdentifierName [

	^ 'ExplicitRequirementMethodsRule'
]

{ #category : 'running' }
ReExplicitRequirementMethodsRule >> check: aClass forCritiquesDo: aCritiqueBlock [
	| explicitRequirementMethods |
	aClass isTrait ifTrue: [ ^ self ].

	"Maybe the subclasses will define the method, so we should not raise a critic on abstract classes."
	aClass isAbstract ifTrue: [ ^ self ].

	explicitRequirementMethods := aClass traitComposition allSelectors
		collect: [ :selector | aClass >> selector ]
		thenSelect: [ :method | method isRequired and: [ method isSubclassResponsibility not ] ].

	explicitRequirementMethods
		select: [ :method | aClass withAllSuperclasses noneSatisfy: [ :superclass | superclass canPerform: method selector ] ]
		thenDo: [ :method |
			aCritiqueBlock
				cull:
					((self critiqueFor: aClass)
						tinyHint: method selector;
						yourself) ]
]
