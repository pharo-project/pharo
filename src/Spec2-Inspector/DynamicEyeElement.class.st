"
I am a dynamic eye element, if you are too lazy to create your own subclass of AbstractEyeElement, use it...
"
Class {
	#name : #DynamicEyeElement,
	#superclass : #AbstractEyeElement,
	#instVars : [
		'descriptionBlock',
		'saveBlock',
		'labelBlock',
		'valueBlock'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
DynamicEyeElement class >> host: anObject label: aLabel description: aBlock value: aValue [ 
	^ (self host: anObject label: aLabel value: aValue)
		description: aBlock;
		yourself
]

{ #category : #'instance creation' }
DynamicEyeElement class >> host: anObject label: aLabel value: aValue [
	^ (self host: anObject)
		label: aLabel;
		value: aValue;
		yourself
]

{ #category : #'instance creation' }
DynamicEyeElement class >> host: anObject value: aValue [
	^ (self host: anObject)
		value: aValue;
		yourself
]

{ #category : #accessing }
DynamicEyeElement >> accessorCode [
	^ self description
]

{ #category : #accessing }
DynamicEyeElement >> description [
	"For big value to print, overwrite description block"
	descriptionBlock ifNil: [ ^ self value asString ].
	^ descriptionBlock cull: self value
]

{ #category : #accessing }
DynamicEyeElement >> description: aBlock [
	descriptionBlock := aBlock
]

{ #category : #accessing }
DynamicEyeElement >> label [
	^ labelBlock value
]

{ #category : #accessing }
DynamicEyeElement >> label: anObject [
	labelBlock := anObject
]

{ #category : #accessing }
DynamicEyeElement >> save: aValue [
	saveBlock value: aValue
]

{ #category : #accessing }
DynamicEyeElement >> saveBlock: anObject [
	saveBlock := anObject
]

{ #category : #accessing }
DynamicEyeElement >> value [
	^ valueBlock value
]

{ #category : #accessing }
DynamicEyeElement >> value: anObject [
	valueBlock := anObject
]
