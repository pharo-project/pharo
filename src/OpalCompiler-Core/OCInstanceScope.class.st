"
I include all variable enries for slots, self, super.
"
Class {
	#name : #OCInstanceScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'vars',
		'selfVar',
		'superVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #acessing }
OCInstanceScope >> allTemps [
	^#()
]

{ #category : #lookup }
OCInstanceScope >> findVariable: lookupBlock ifNone: notFoundBlock [ 

	(lookupBlock value: selfVar) ifTrue: [ ^ selfVar ].
	(lookupBlock value: superVar) ifTrue: [ ^ superVar ].
	
	vars valuesDo: [ :var | (lookupBlock value: var) ifTrue: [ ^ var ] ].

	^ outerScope findVariable: lookupBlock ifNone: notFoundBlock 
]

{ #category : #initialization }
OCInstanceScope >> initialize [
  
	vars := Dictionary new.
	selfVar := OCSelfVariable new
		scope: self;
		yourself.
	superVar := OCSuperVariable new
		scope: self;
		yourself.
]

{ #category : #acessing }
OCInstanceScope >> instanceScope [
	^self
]

{ #category : #testing }
OCInstanceScope >> isInstanceScope [

	^ true
]

{ #category : #lookup }
OCInstanceScope >> lookupVar: name [
	"Return a ScopeVar for my inst var with this name.  Return nil if none found"

	name = 'self' ifTrue: [^ selfVar].
	name = 'super' ifTrue: [^ superVar].
	^ vars at: name ifAbsent: [self outerScope lookupVar: name]
]

{ #category : #lookup }
OCInstanceScope >> lookupVarForDeclaration: name [
	"Return a ScopeVar for my inst var with this name.  Return nil if none found"
	name = 'self' ifTrue: [^ selfVar].
	name = 'super' ifTrue: [^ superVar].
	^ vars at: name ifAbsent: [self outerScope lookupVarForDeclaration: name]
]

{ #category : #scope }
OCInstanceScope >> newMethodScope [

	^ OCMethodScope new outerScope: self
]

{ #category : #initializing }
OCInstanceScope >> slots: slotCollection [

	slotCollection do: [ :slot |
		vars at: slot name put:
			(OCSlotVariable new
				slot: slot;
				scope: self;
				yourself)].

]
