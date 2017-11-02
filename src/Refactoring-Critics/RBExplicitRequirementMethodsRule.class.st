"
Classes that use traits with explicit requirement methods should either implement the method or inherit it.
"
Class {
	#name : #RBExplicitRequirementMethodsRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
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
RBExplicitRequirementMethodsRule >> basicCheck: aClass [ 

	| explicitRequirementMethods |
	explicitRequirementMethods := Set new.
	
	aClass isTrait 
		ifTrue: [ ^ false ].
		
	aClass traitComposition allSelectors do: [ :selector | | method |
				method := aClass>>selector.
				 (method isRequired and: [ method isSubclassResponsibility not ]) 
						ifTrue: [ explicitRequirementMethods add: method ]].
	
	^ explicitRequirementMethods anySatisfy: [ :method |
		aClass withAllSuperclasses 
			noneSatisfy: [ :superclass | superclass canPerform: method selector ] ].
]

{ #category : #running }
RBExplicitRequirementMethodsRule >> checkClass: aClass [ 

	| explicitRequirementMethods |
	explicitRequirementMethods := Set new.
	aClass isTrait 
		ifTrue: [ ^ self ].
		
	aClass traitComposition allSelectors do: [ :selector | | method |
				method := aClass>>selector.
				 (method isRequired and: [ method isSubclassResponsibility not ]) 
						ifTrue: [ explicitRequirementMethods add: method ]].
	
	^ explicitRequirementMethods do: [ :method |
		aClass withAllSuperclasses 
			detect: [ :superclass | superclass canPerform: method selector ] ifNone: [result addClass: aClass selector: method selector]].
]

{ #category : #'as yet unclassified' }
RBExplicitRequirementMethodsRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBExplicitRequirementMethodsRule >> name [
	^ 'Explicit requirement methods'
]

{ #category : #accessing }
RBExplicitRequirementMethodsRule >> rationale [
	^ 'Classes that use traits with explicit requirement methods should either implement the method or inherit it.'
]
