"
I include all variable enries for slots, self, super.
"
Class {
	#name : #OCInstanceScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'vars',
		'reservedVars'
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
	
	(reservedVars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	(vars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	
	^ self outerScope hasBindingThatBeginsWith: aString
]

{ #category : #initialization }
OCInstanceScope >> initialize [
  
	vars := Dictionary new.
	reservedVars  := (ReservedVariable subclasses collect: [ :class |
			class instance name -> class instance]) asDictionary 
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
	reservedVars at: name ifPresent: [:v | ^ v].
	^ vars at: name ifAbsent: [self outerScope lookupVar: name]
]

{ #category : #lookup }
OCInstanceScope >> lookupVarForDeclaration: name [
	"This is a version of #lookupVar: that skips the OCRequestorScope"
	reservedVars at: name ifPresent: [:v | ^ v].
	^ vars at: name ifAbsent: [self outerScope lookupVarForDeclaration: name]
]

{ #category : #scope }
OCInstanceScope >> newMethodScope [

	^ OCMethodScope new outerScope: self
]

{ #category : #acessing }
OCInstanceScope >> scopeLevel [
	"For debugging we print a counter for all scopes, starting from the Method as 1, so we are 0"
	^ 0
]

{ #category : #initializing }
OCInstanceScope >> slots: slotCollection [

	slotCollection do: [ :slot |
		vars at: slot name put: slot]
]
