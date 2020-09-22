"
I include all variable enries for slots, self, super.
"
Class {
	#name : #OCInstanceScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'vars'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #acessing }
OCInstanceScope >> allTemps [
	^#()
]

{ #category : #lookup }
OCInstanceScope >> hasBindingThatBeginsWith: aString [
	"Answer true if a reserved variable or any slot start with aString"
	(vars hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	
	^ self outerScope hasBindingThatBeginsWith: aString
]

{ #category : #initialization }
OCInstanceScope >> initialize [
 	"we pre-fill the reserved vars, more get added by the semantic analysis"
	vars := ReservedVariable lookupDictionary
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
	"Return a ScopeVar for my inst var with this name. Return nil if none found"
	^ vars at: name ifAbsent: [self outerScope lookupVar: name]
]

{ #category : #lookup }
OCInstanceScope >> lookupVarForDeclaration: name [
	"This is a version of #lookupVar: that skips the OCRequestorScope"
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
