"
Some classes have special importance to the core system.  DangerousClassNotifier notifies users which class defiitions should not be modified. 

This class is refactored from the following Pharo2 methods:

* Behaviour>>shouldNotBeRedefined

* ClassBuilder>>tooDangerousClasses

* ClassBuilder>>name:  inEnvironment:  subclassOf:  type:  instanceVariableNames:  classVariableNames:  poolDictionaries:  category:  unsafe: 

Instance Variables
	enabled:		<Boolean>
	restoreState:		<Boolean>

enabled
	- Specifies whether this checking is performed. Some system tests that check ""dangerous"" behaviour need this turned off. 

restoreState
	- After disabling for testing, need to restore the previous state. 

"
Class {
	#name : #DangerousClassNotifier,
	#superclass : #Object,
	#classInstVars : [
		'enabled'
	],
	#category : #Slot
}

{ #category : #validation }
DangerousClassNotifier class >> check: classSymbol [

	self enabled ifFalse: [ ^false ].
	
	(self shouldNotBeRedefined: classSymbol)		
			ifTrue: [ self notify: '"' , classSymbol , '" should not be redefined as its structure is known to the VM. \Only proceed if you know what you are doing!' withCRs ].


]

{ #category : #accessing }
DangerousClassNotifier class >> disable [
	enabled := false.
	
	
]

{ #category : #accessing }
DangerousClassNotifier class >> disableDuring: aBlock [
	"execute a block with me disabled. Works even nested and restores the old state"
	| oldState |
	oldState := enabled.
	enabled := false.
	aBlock value.
	enabled := oldState.
]

{ #category : #accessing }
DangerousClassNotifier class >> enable [
	enabled := true.
	
	
]

{ #category : #accessing }
DangerousClassNotifier class >> enabled [
	^enabled ifNil: [  enabled := false ]

]

{ #category : #accessing }
DangerousClassNotifier class >> initialize [
	enabled := true.
]

{ #category : #accessing }
DangerousClassNotifier class >> shouldNotBeRedefined: classSymbol [
	(self tooDangerousClasses includes: classSymbol) ifTrue: [ ^true ].
	^Smalltalk globals
		at: classSymbol 
		ifPresent: [ :class | (Smalltalk specialObjectsArray includes: class) or: [class isKindOf: class]]
		ifAbsent: false .

]

{ #category : #accessing }
DangerousClassNotifier class >> tooDangerousClasses [
	"Return a list of class names which will not be modified in the public interface"
	^#(
		"Object will break immediately"
		#ProtoObject #Object
		"Contexts and their superclasses"
		#InstructionStream #Context #BlockClosure #CompiledMethod #CompiledCode #CompiledBlock
	)
]
