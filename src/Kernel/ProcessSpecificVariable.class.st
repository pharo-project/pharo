"
My subclasses (not instances of them) keep state specific to the current process.

There are two kinds of process-specific variables: process-local (state available
for read and write in all methods inside the process), and dynamic variables
(implementing dynamic scope).

My subclasses could supply inheritable values which will be installed on forked (child) processes. To enable this classes should return true from  #isInheritable method (on class side).
Also subclasses could provide specific logic for installing variables into new processes which allows interesting hooks to control forked processes
"
Class {
	#name : #ProcessSpecificVariable,
	#superclass : #Object,
	#instVars : [
		'index'
	],
	#classInstVars : [
		'soleInstance'
	],
	#category : #Kernel-Processes
}

{ #category : #'class initialization' }
ProcessSpecificVariable class >> initialize [
	self resetSoleInstance.
]

{ #category : #testing }
ProcessSpecificVariable class >> isInheritable [
	^false
]

{ #category : #'instance creation' }
ProcessSpecificVariable class >> new [
	| instance |
	instance := super new.
	instance index: (Process allocatePSKey: instance).
	^ instance
]

{ #category : #'class initialization' }
ProcessSpecificVariable class >> resetSoleInstance [
	soleInstance := nil.
]

{ #category : #accessing }
ProcessSpecificVariable class >> soleInstance [
	^ soleInstance ifNil: [ soleInstance := self new ]
]

{ #category : #accessing }
ProcessSpecificVariable class >> value [
	"Answer the current value for this variable in the current context."
	^ self soleInstance value
]

{ #category : #accessing }
ProcessSpecificVariable >> default [
	"Answer the default value for the variable. The default for the default value is nil."
	^nil
]

{ #category : #accessing }
ProcessSpecificVariable >> index [
	^ index
]

{ #category : #accessing }
ProcessSpecificVariable >> index: anInteger [
	index := anInteger
]

{ #category : #inheriting }
ProcessSpecificVariable >> installValue: anObject intoForked: newProcess from: ownerProcess [
"
	In subclasses you can override this method to implement specific variable inheritance logic
"
	newProcess psValueAt: index put: anObject
]

{ #category : #inheriting }
ProcessSpecificVariable >> isInheritable [
	^self class isInheritable
]

{ #category : #accessing }
ProcessSpecificVariable >> value [

	^ (Processor activeProcess psValueAt: index) ifNil: [ self default ]
]

{ #category : #accessing }
ProcessSpecificVariable >> valueOrNil [
	"a faster version, which doesn't using ifAbsent: to avoid using block closure"
	^ Processor activeProcess psValueAt: index
]
