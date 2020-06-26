"
I include all variable enries for class, pool and global vars.
"
Class {
	#name : #OCClassScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'class'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'instance creation' }
OCClassScope class >> for: aClass [
	^self new 
		class: aClass;
		yourself
]

{ #category : #accessing }
OCClassScope >> class: aBehavior [

	class := aBehavior
]

{ #category : #accessing }
OCClassScope >> environment [
	^class classPool
]

{ #category : #accessing }
OCClassScope >> getClass [
	^class
]

{ #category : #lookup }
OCClassScope >> hasBindingThatBeginsWith: aString [
	"Return a SemVar for my pool var with this name.  Return nil if none found"

	(class hasBindingThatBeginsWith: aString) ifTrue: [ ^true ].
	^outerScope hasBindingThatBeginsWith: aString
]

{ #category : #levels }
OCClassScope >> instanceScope [

	^ OCInstanceScope new
		slots: class allSlots;
		outerScope: self;
		yourself
]

{ #category : #lookup }
OCClassScope >> lookupVar: name [
	"Return a SemVar for my pool var with this name.  Return nil if none found"

	^(class innerBindingOf: name) 
		ifNotNil: [:var | ^var]
		ifNil: [ outerScope lookupVar: name] 
]

{ #category : #lookup }
OCClassScope >> lookupVarForDeclaration: name [
	"This is a version of #lookupVar: that skips the OCRequestorScope"
	^self lookupVar: name
]

{ #category : #levels }
OCClassScope >> newMethodScope [
	^ self instanceScope newMethodScope
]

{ #category : #printing }
OCClassScope >> printOn: stream [

	class printOn: stream.
]
