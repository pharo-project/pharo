"
When loading a class where a Slot is used that is not present in the System, we need to model
this slot somehow.

The UndefinedSlot ingores reads and write (it returns nil like undeclared variables). But on
access, it checks if the Slot has been loaded and if yes, it rebuilds the class definition.
"
Class {
	#name : #UndefinedSlot,
	#superclass : #Slot,
	#instVars : [
		'ast',
		'classIsRebuild'
	],
	#classInstVars : [
		'undeclaredSlots'
	],
	#category : #'Kernel-Variables'
}

{ #category : #'instance creation' }
UndefinedSlot class >> named: aName ast: aSlotClassName [
	^ (self named: aName) ast: aSlotClassName
]

{ #category : #accessing }
UndefinedSlot >> ast: aMessageNode [
	classIsRebuild := false.
	ast := aMessageNode
]

{ #category : #private }
UndefinedSlot >> checkClassRebuild [
	"break the recursion while rebuilding"
	classIsRebuild ifTrue: [ ^ self].
	(self definingClass environment hasClassNamed: self slotClassName) ifFalse: [ ^ self ].
	classIsRebuild := true.
	"we rebuild the class, this triggers instance migration"
	self definingClass compiler evaluate: self definingClass definitionWithSlots.
	"recompile all methods that access me to generat code for the loaded definition"
	self usingMethods do: [:each | each recompile].
]

{ #category : #printing }
UndefinedSlot >> printOn: aStream [
	"we print as the definition that could not be loaded"
	aStream nextPutAll: ast formattedCode
]

{ #category : #'meta-object-protocol' }
UndefinedSlot >> read: anObject [
	"Undeclared slots read nil always, but check if they can repair the class"
	 self checkClassRebuild.
	^ nil
]

{ #category : #accessing }
UndefinedSlot >> slotClassName [
	^ast arguments first variable name 
]

{ #category : #'meta-object-protocol' }
UndefinedSlot >> write: aValue to: anObject [
	"Undeclared slots ignore writes, but check if they can repair the class"
	 self checkClassRebuild.
	^ aValue
]
