"
I add some special behavior: 
	- I override bytecode generation to generate ivar read and write bytecodes
	- I print the definition as #name 
	
If you subclass this class, take note that as it overrides both #emitStore: and #emitValue: to use the fast bytecode, it will *not* call the reflective #read: and #write:to: methods.


"
Class {
	#name : #InstanceVariableSlot,
	#superclass : #IndexedSlot,
	#category : #'Slot-Core-Variables'
}

{ #category : #cleanup }
InstanceVariableSlot class >> resetIvarSlots [
	"when the Ivar slots need to be re-created (e.g. due to changes in the layout of the class),
	 this method re-creates for every ivar a ivar slot"
	"to be removed later when the system is in a stable state"

	| block |
	block := [ :class | 
	class instVarNames
		withIndexDo: [ :ivarName :index | 
			class classLayout slotScope
				at: index
				put:
					((InstanceVariableSlot named: ivarName asSymbol)
						index: index + class superclass allInstVarNames size;
						yourself) ] ].
	Smalltalk allClasses
		do: [ :class | 
			block value: class.
			block value: class classSide ]
]

{ #category : #printing }
InstanceVariableSlot >> definitionString [
	"non special globals are defined by the symbol"
	^self needsFullDefinition
		ifTrue: [ super definitionString ]
		ifFalse: [ self name printString ]

]

{ #category : #'code generation' }
InstanceVariableSlot >> emitStore: methodBuilder [
	"generate store bytecode"
	methodBuilder storeInstVar: index
]

{ #category : #'code generation' }
InstanceVariableSlot >> emitValue: methodBuilder [
	"emit the bytecode to push ivar"
	methodBuilder pushInstVar: index

]

{ #category : #testing }
InstanceVariableSlot >> isAccessedIn: aCompiledCode [
	^(aCompiledCode readsField: index) or: [aCompiledCode writesField: index]
]

{ #category : #testing }
InstanceVariableSlot >> isReadIn: aCompiledCode [
	^aCompiledCode readsField: index
]

{ #category : #testing }
InstanceVariableSlot >> isWrittenIn: aCompiledCode [
	^aCompiledCode writesField: index
]

{ #category : #testing }
InstanceVariableSlot >> needsFullDefinition [
	"I am just a backward compatible ivar slot and can use simple definitions.
	 Note: my subclasses need full definitions"

	^ self class ~= InstanceVariableSlot
]
