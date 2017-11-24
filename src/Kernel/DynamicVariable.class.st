"
My subclasses are dynamic variables: each subclass represents a variable
whose value persists inside the block passed to #value:during:. There is
no way to change the value inside such a block, but it is possible to
temporarirly rebind it in a nested manner.
"
Class {
	#name : #DynamicVariable,
	#superclass : #ProcessSpecificVariable,
	#category : #'Kernel-Processes'
}

{ #category : #accessing }
DynamicVariable class >> value: anObject during: aBlock [

	^ self soleInstance value: anObject during: aBlock
]

{ #category : #accessing }
DynamicVariable >> value: anObject during: aBlock [
	| activeProcess oldValue |
	activeProcess := Processor activeProcess.
	"oldValue will be nil if the variable has not yet been set"
	oldValue := activeProcess psValueAt: index.
	^ [ activeProcess
			psValueAt: index
			put: anObject.
		aBlock value ] ensure: [ 
			activeProcess
				psValueAt: index
				put: oldValue ]
]
