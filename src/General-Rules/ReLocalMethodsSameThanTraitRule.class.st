"
If a class has a method in its trait composition, the method should not be implemented in the class.
"
Class {
	#name : 'ReLocalMethodsSameThanTraitRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'testing' }
ReLocalMethodsSameThanTraitRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReLocalMethodsSameThanTraitRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReLocalMethodsSameThanTraitRule class >> ruleName [
	^ 'Repeated methods in the trait composition'
]

{ #category : 'accessing' }
ReLocalMethodsSameThanTraitRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'MethodInClassVsTrait'
]

{ #category : 'running' }
ReLocalMethodsSameThanTraitRule >> check: aClass forCritiquesDo: aCriticBlock [
	"The comparison between methods is made using the ast, this is better than comparing source code only since it does not take into account identations, extra parenthesis, etc"

	aClass isTrait ifTrue: [ ^ self ].
	aClass hasTraitComposition ifFalse: [ ^ self ].
	aClass localMethods
		select: [ :method |
			| traitCompositionMethod |
			traitCompositionMethod := aClass traitComposition compiledMethodAt: method selector ifAbsent: [ nil ].
			traitCompositionMethod isNotNil and: [ traitCompositionMethod ast = method ast ] ]
		thenDo: [ :duplicatedMethod |
			aCriticBlock cull:
				(ReRemoveMethodCritique for: aClass selector: duplicatedMethod selector by: self) ]
]
