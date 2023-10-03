"
I can be used to reify the condition of a link.

I have myself a condition *and* I store the condtition of the link.

on #value, I evaluate my condition and after that the orginal link condition.

With this feature, we can thus reflectively controll links while not actually changing the condition of the link itself. 
"
Class {
	#name : 'RFCondition',
	#superclass : 'Object',
	#instVars : [
		'condition',
		'originalLinkCondition'
	],
	#category : 'Reflectivity-Core',
	#package : 'Reflectivity',
	#tag : 'Core'
}

{ #category : 'instance creation' }
RFCondition class >> for: aCondition [
	^self new originalLinkCondition: aCondition
]

{ #category : 'accessing' }
RFCondition >> condition: aBlockOrBoolean [
	condition := aBlockOrBoolean
]

{ #category : 'evaluating' }
RFCondition >> cull: arguments [
	"in case of my wrapped link expects arguments"

	^ condition
		and: [ self linkCondition perform: self linkCondition valueSelector withArguments: {arguments} ]
]

{ #category : 'enable/disable' }
RFCondition >> disable [
	condition := false
]

{ #category : 'enable/disable' }
RFCondition >> enable [
	condition := true
]

{ #category : 'initialization' }
RFCondition >> initialize [
	condition := true
]

{ #category : 'accessing' }
RFCondition >> linkCondition [
	^ self originalLinkCondition ifNil: [ [ true ] ]
]

{ #category : 'accessing' }
RFCondition >> originalLinkCondition [
	^ originalLinkCondition
]

{ #category : 'accessing' }
RFCondition >> originalLinkCondition: aCondition [
	originalLinkCondition := aCondition
]

{ #category : 'accessing' }
RFCondition >> reifications [
	(condition isKindOf: Boolean) ifTrue: [ ^#() ].
	^condition argumentNames
]

{ #category : 'evaluating' }
RFCondition >> value [
	^condition and: [ self linkCondition value ]
]

{ #category : 'accessing' }
RFCondition >> valueSelector [
	"If the condition is a block with 1+ arguments, it must be sent #cull: to be evaluated. Otherwise, it's a block with no argument or directly true or false, so we send it #value to evaluate it"
	^ (self linkCondition isBlock and: [ self linkCondition numArgs isZero not ] )
		ifTrue: [ #cull: ]
		ifFalse: [ #value ]
]
