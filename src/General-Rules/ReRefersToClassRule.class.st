"
This smell arises when a class has its class name directly in the source instead of ""self class"" (or just ""self"" when on the class side). The self class / self variant allows you to create subclasses without needing to redefine that method.
However we cannot systematically replace Class reference by self class or self because a Class reference is static and a self expression is dynamic. So the programmer may want to send messages to root of an hierarchy and not to the leaf classes. Therefore this rule generates false positives, please double check when fixing!
"
Class {
	#name : 'ReRefersToClassRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReRefersToClassRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReRefersToClassRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReRefersToClassRule class >> rationale [
	^ 'Checks for direct reference to classes themselves.'
]

{ #category : 'accessing' }
ReRefersToClassRule class >> ruleName [
	^ 'Refers to class name instead of "self class"'
]

{ #category : 'accessing' }
ReRefersToClassRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'RefersToClassRule'
]

{ #category : 'running' }
ReRefersToClassRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| class problemLiterals |

	"The class method #isAbstract needs to explicitly hardcode class references.
	So we do not criticize this."
	(aMethod isClassSide and: [ aMethod selector = #isAbstract ]) ifTrue: [ ^ self ].

	class := aMethod methodClass instanceSide.
	problemLiterals := (aMethod ast allChildren
			select: [ :node | node isGlobalVariable ])
			select: [ :var | var name = class name].

	problemLiterals do: [ :literal |
			aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: literal hint:literal name )]
]
