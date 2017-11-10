"
I represent a field in a structure layout. I am used for computing which fields present in a structure are passed in registers, and which are not.
"
Class {
	#name : #FFIExternalStructureFlatLayoutField,
	#superclass : #Object,
	#instVars : [
		'registerClass',
		'offset',
		'size',
		'alignment'
	],
	#category : #UnifiedFFI
}

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> alignment [
	^ alignment
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> alignment: anObject [
	alignment := anObject
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> offset [
	^ offset
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> offset: anObject [
	offset := anObject
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> registerClass [
	^ registerClass
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> registerClass: anObject [
	registerClass := anObject
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> size [
	^ size
]

{ #category : #accessing }
FFIExternalStructureFlatLayoutField >> size: anObject [
	size := anObject
]
