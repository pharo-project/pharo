"
If a class has a method in its trait composition, the method should not be implemented in the class.
"
Class {
	#name : #RBLocalMethodsOfAClassNotInItsTraitComposition,
	#superclass : #ReAbstractRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #testing }
RBLocalMethodsOfAClassNotInItsTraitComposition class >> checksClass [
	^ true
]

{ #category : #accessing }
RBLocalMethodsOfAClassNotInItsTraitComposition class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MethodInClassVsTrait'
]

{ #category : #running }
RBLocalMethodsOfAClassNotInItsTraitComposition >> check: aClass forCritiquesDo: aCriticBlock [
	
	"The comparison between methods is made using the ast, this is better than comparing source code only since it does not take into account identations, extra parenthesis, etc"

	(aClass isTrait not and: [ aClass hasTraitComposition and: [  
		aClass localMethods anySatisfy: [ :method | |traitCompositionMethod |
			traitCompositionMethod := (aClass traitComposition methodDescriptionForSelector: method selector) effectiveMethod.
			traitCompositionMethod notNil and: [
				(traitCompositionMethod ast = method ast) ] ] ] ]) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aClass) ]
]

{ #category : #'as yet unclassified' }
RBLocalMethodsOfAClassNotInItsTraitComposition >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBLocalMethodsOfAClassNotInItsTraitComposition >> name [
	^ 'Repeated methods in the trait composition'
]
