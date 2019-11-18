"
Classes that use traits with explicit requirement methods should either implement the method or inherit it.
"
Class {
	#name : #RBExplicitRequirementMethodsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBExplicitRequirementMethodsRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBExplicitRequirementMethodsRule class >> uniqueIdentifierName [

	^ 'ExplicitRequirementMethodsRule'
]

{ #category : #running }
RBExplicitRequirementMethodsRule >> check: aClass forCritiquesDo: aCritiqueBlock [
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

{ #category : #accessing }
RBExplicitRequirementMethodsRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBExplicitRequirementMethodsRule >> name [
	^ 'Explicit requirement methods'
]
