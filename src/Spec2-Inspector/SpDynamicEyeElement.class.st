"
I am a dynamic eye element, if you are too lazy to create your own subclass of AbstractEyeElement, use it...
"
Class {
	#name : #SpDynamicEyeElement,
	#superclass : #SpAbstractEyeElement,
	#instVars : [
		'descriptionBlock',
		'saveBlock',
		'labelBlock',
		'valueBlock'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpDynamicEyeElement class >> host: anObject label: aLabel description: aBlock value: aValue [ 
	^ (self host: anObject label: aLabel value: aValue)
		description: aBlock;
		yourself
]

{ #category : #'instance creation' }
SpDynamicEyeElement class >> host: anObject label: aLabel value: aValue [
	^ (self host: anObject)
		label: aLabel;
		value: aValue;
		yourself
]

{ #category : #'instance creation' }
SpDynamicEyeElement class >> host: anObject value: aValue [
	^ (self host: anObject)
		value: aValue;
		yourself
]

{ #category : #accessing }
SpDynamicEyeElement >> accessorCode [
	^ self description
]

{ #category : #accessing }
SpDynamicEyeElement >> description [
	"For big value to print, overwrite description block"
	descriptionBlock ifNil: [ ^ self value asString ].
	^ descriptionBlock cull: self value
]

{ #category : #accessing }
SpDynamicEyeElement >> description: aBlock [
	descriptionBlock := aBlock
]

{ #category : #accessing }
SpDynamicEyeElement >> label [
	^ labelBlock value
]

{ #category : #accessing }
SpDynamicEyeElement >> label: anObject [
	labelBlock := anObject
]

{ #category : #accessing }
SpDynamicEyeElement >> save: aValue [
	saveBlock value: aValue
]

{ #category : #accessing }
SpDynamicEyeElement >> saveBlock: anObject [
	saveBlock := anObject
]

{ #category : #accessing }
SpDynamicEyeElement >> value [
	^ valueBlock value
]

{ #category : #accessing }
SpDynamicEyeElement >> value: anObject [
	valueBlock := anObject
]
