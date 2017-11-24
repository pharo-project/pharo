"
I can be used to reify the condition of a link.

I have myself a condition *and* I store the condtition of the link.

on #value, I evaluate my condition and after that the orginal link condition.

With this feature, we can thus reflectively controll links while not actually changing the condition of the link itself. 
"
Class {
	#name : #RFCondition,
	#superclass : #Object,
	#instVars : [
		'condition',
		'linkCondition'
	],
	#category : #'Reflectivity-Links'
}

{ #category : #'instance creation' }
RFCondition class >> for: aCondition [
	^self new linkCondition: aCondition
]

{ #category : #accessing }
RFCondition >> condition: aBlockOrBoolean [
	condition := aBlockOrBoolean
]

{ #category : #evaluating }
RFCondition >> cull: arguments [
	"in case of my wrapped link expects arguments"

	^ condition
		and: [ linkCondition perform: linkCondition valueSelector withArguments: {arguments} ]
]

{ #category : #'enable/disable' }
RFCondition >> disable [
	condition := false.
]

{ #category : #'enable/disable' }
RFCondition >> enable [
	condition := true
]

{ #category : #initialization }
RFCondition >> initialize [
	condition := true
]

{ #category : #accessing }
RFCondition >> linkCondition: aBlockOrBoolean [
		linkCondition := aBlockOrBoolean
]

{ #category : #accessing }
RFCondition >> reifications [
	(condition isKindOf: Boolean) ifTrue: [ ^#() ].
	^condition argumentNames. 
]

{ #category : #evaluating }
RFCondition >> value [
	^condition and: [ linkCondition value ]
]

{ #category : #accessing }
RFCondition >> valueSelector [
	linkCondition numArgs isZero
		ifTrue: [ ^ #value ]
		ifFalse: [ ^ #cull: ]
]
