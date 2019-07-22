"
If a class has a method in its trait composition, the method should not be implemented in the class.
"
Class {
	#name : #RBLocalMethodsSameThanTraitRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBLocalMethodsSameThanTraitRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBLocalMethodsSameThanTraitRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MethodInClassVsTrait'
]

{ #category : #running }
RBLocalMethodsSameThanTraitRule >> check: aClass forCritiquesDo: aCriticBlock [
	"The comparison between methods is made using the ast, this is better than comparing source code only since it does not take into account identations, extra parenthesis, etc"

	aClass isTrait ifTrue: [ ^ self ].
	aClass hasTraitComposition ifFalse: [ ^ self ].
	aClass localMethods
		select: [ :method | 
			| traitCompositionMethod |
			traitCompositionMethod := aClass traitComposition compiledMethodAt: method selector ifAbsent: [ nil ].
			traitCompositionMethod notNil and: [ traitCompositionMethod ast = method ast ] ]
		thenDo: [ :duplicatedMethod |
			aCriticBlock cull:
				(ReRemoveMethodCritique for: aClass selector: duplicatedMethod selector by: self) ]
]

{ #category : #accessing }
RBLocalMethodsSameThanTraitRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBLocalMethodsSameThanTraitRule >> name [
	^ 'Repeated methods in the trait composition'
]
