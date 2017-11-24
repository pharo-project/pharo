"
I implement a filtering strategy for processors based on method signatures (method selector and class).

I only allow processors created by a method currently added to the filter.

I can attach to each processor a configuration block.

For examples view my class side methods in teh category 'example'
"
Class {
	#name : #GTSpotterMethodListProcessorFilter,
	#superclass : #GTSpotterProcessorFilter,
	#instVars : [
		'selectedMethodSignatures',
		'configurationBlocks'
	],
	#category : #'GT-Spotter-Model'
}

{ #category : #adding }
GTSpotterMethodListProcessorFilter >> allowProcessorsFrom: aMethod [
	|signature|
	
	signature := self signatureFor: aMethod.
	(selectedMethodSignatures includes: signature) ifFalse: [ selectedMethodSignatures add: signature ].
	
]

{ #category : #adding }
GTSpotterMethodListProcessorFilter >> allowProcessorsFrom: aMethod configuredWith: aBlock [
	
	self allowProcessorsFrom: aMethod.
	configurationBlocks at: aMethod printString put: aBlock.
	
]

{ #category : #accessing }
GTSpotterMethodListProcessorFilter >> configurationBlockForProcessorsCreatedBy: aMethod [
	
	^ configurationBlocks at: aMethod printString ifAbsent: [ self nullConfigurationBlock ]
]

{ #category : #initialization }
GTSpotterMethodListProcessorFilter >> initialize [
	super initialize.

	selectedMethodSignatures := OrderedCollection new.
	configurationBlocks := Dictionary new.
]

{ #category : #accessing }
GTSpotterMethodListProcessorFilter >> nullConfigurationBlock [

	^ [ :processor | ]
]

{ #category : #printing }
GTSpotterMethodListProcessorFilter >> printSignature: aSignature [

	^ aSignature first name, '>>', aSignature second asString
]

{ #category : #testing }
GTSpotterMethodListProcessorFilter >> shouldEnableProcessorsCreateBy: aMethod [

	^ selectedMethodSignatures anySatisfy: [ :aSignature | 
		aSignature first = aMethod methodClass and: [ aSignature second = aMethod selector ] ]
]

{ #category : #accessing }
GTSpotterMethodListProcessorFilter >> signatureFor: aMethod [

	^ {aMethod methodClass . aMethod selector}
]
