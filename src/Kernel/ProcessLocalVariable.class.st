"
My subclasses have values specific to the active process. They can be read with #value and set with #value:
"
Class {
	#name : #ProcessLocalVariable,
	#superclass : #ProcessSpecificVariable,
	#category : #Kernel-Processes
}

{ #category : #accessing }
ProcessLocalVariable class >> value: anObject [
	^ self soleInstance value: anObject
]

{ #category : #evaluating }
ProcessLocalVariable >> value: anObject [
	Processor activeProcess psValueAt: index put: anObject
]
