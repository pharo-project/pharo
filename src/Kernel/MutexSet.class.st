"
A MutexSet helps with aquiring a set of mutexes.
"
Class {
	#name : #MutexSet,
	#superclass : #Object,
	#instVars : [
		'array'
	],
	#category : #'Kernel-Processes'
}

{ #category : #'instance creation' }
MutexSet class >> withAll: mutexList [
	^self new withAll: mutexList
]

{ #category : #'mutual exclusion' }
MutexSet >> critical: aBlock [
	"Evaluate aBlock aquiring all mutexes"
	^self pvtCritical: aBlock startingAt: 1
]

{ #category : #private }
MutexSet >> pvtCritical: aBlock startingAt: index [
	| mutex |
	index > array size ifTrue:[^aBlock value].
	mutex := array at: index.
	^mutex critical:[self pvtCritical: aBlock startingAt: index+1].
]

{ #category : #initialize }
MutexSet >> withAll: mutexList [
	array := mutexList.
]
