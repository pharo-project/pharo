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
OCInstanceScope >> hasBindingThatBeginsWith: aString [
	"Answer true if true, false or any slot start with aString"

	(selfVar name beginsWith: aString) ifTrue: [ ^true ].
	(superVar name beginsWith: aString) ifTrue: [ ^true ].
	(vars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	
	^ self outerScope hasBindingThatBeginsWith: aString
]

{ #category : #initialization }
OCInstanceScope >> initialize [
  
	vars := Dictionary new.
	selfVar := SelfVariable instance.
	superVar := SuperVariable instance
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

	name = selfVar name ifTrue: [^ selfVar].
	name = superVar name ifTrue: [^ superVar].
	^ vars at: name ifAbsent: [self outerScope lookupVar: name]
]

{ #category : #lookup }
OCInstanceScope >> lookupVarForDeclaration: name [
	"This is a version of #lookupVar: that skips the OCRequestorScope"
	name = selfVar name ifTrue: [^ selfVar].
	name = superVar name ifTrue: [^ superVar].
	^ vars at: name ifAbsent: [self outerScope lookupVarForDeclaration: name]
]

{ #category : #scope }
OCInstanceScope >> newMethodScope [

	^ OCMethodScope new outerScope: self
]

{ #category : #initializing }
OCInstanceScope >> slots: slotCollection [

	slotCollection do: [ :slot |
		vars at: slot name put: slot]
]
