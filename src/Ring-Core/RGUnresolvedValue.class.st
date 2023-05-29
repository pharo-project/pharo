Class {
	#name : #RGUnresolvedValue,
	#superclass : #Object,
	#instVars : [
		#default
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGUnresolvedValue class >> recursive [

	| unresolvedValue |

	unresolvedValue := self new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue
]

{ #category : #accessing }
RGUnresolvedValue >> default [
	^ default
]

{ #category : #accessing }
RGUnresolvedValue >> default: anObject [
	default := anObject
]

{ #category : #testing }
RGUnresolvedValue >> hasResolvedName [

	^ false
]

{ #category : #testing }
RGUnresolvedValue >> isRGUnresolvedValue [

	^ true
]

{ #category : #testing }
RGUnresolvedValue >> isRingFullyResolved [

	^ false
]

{ #category : #testing }
RGUnresolvedValue >> isRingFullyUnresolved [

	^ true
]

{ #category : #testing }
RGUnresolvedValue >> isRingResolved [

	^ false
]

{ #category : #accessing }
RGUnresolvedValue >> orDefaultForUnresolved [

	^ self default
]

{ #category : #accessing }
RGUnresolvedValue >> referencedBehaviors [

	^ #()
]

{ #category : #accessing }
RGUnresolvedValue >> value [

	^ self default
]
