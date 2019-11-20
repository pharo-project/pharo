Class {
	#name : #GTSpotterTest,
	#superclass : #TestCase,
	#instVars : [
		'spotter'
	],
	#category : #'GT-Tests-Spotter-Scripting'
}

{ #category : #assertions }
GTSpotterTest >> assertText: aTextualObject [
	self assertText: aTextualObject do: nil
]

{ #category : #assertions }
GTSpotterTest >> assertText: aTextualObject do: aBlock [
	| result |
	" self: emulate a view. Forget about a separate GTSpotterMockView. kiss :) "
	self shouldnt: [ result := spotter setText: aTextualObject from: self ] raise: Error.

	self assert: result.
	self deny: spotter hasSteps not.
	self denyEmpty: spotter steps.

	self shouldnt: [ aBlock value ] raise: Error
]

{ #category : #running }
GTSpotterTest >> setUp [ 

	super setUp.
	
	spotter := GTMockSpotter new
		exceptionHandler: GTSpotterDebuggingExceptionHandler new;
		yourself
]

{ #category : #tests }
GTSpotterTest >> testEmptyTextInput [
	
	self assertText: Text new do: [ self assert: spotter text isEmpty ].
	self assertText: (Text fromString: '') do: [ self assert: spotter text isEmpty ].
	self assertText: '' do: [ self assert: spotter text isEmpty ].
	self flag: 'currently: nil is converted to the string "nil"'
	"self assertText: nil do: [ self assert: spotter text isEmpty ]."
]

{ #category : #tests }
GTSpotterTest >> testHistorizingTheSameObjectRemovesPreviousOccurences [
	| history |
	history := OrderedCollection new.
	spotter := GTSpotter new history: history.
	spotter historize: (GTSpotterCandidateLink new value: 42).
	self assert: history size equals: 1.
	spotter historize: (GTSpotterCandidateLink new value: 43).
	self assert: history size equals: 2.
	self assert: history first value equals: 43.
	self assert: history last value equals: 42.
	spotter historize: (GTSpotterCandidateLink new value: 42).
	self assert: history size equals: 2.
	self assert: history first value equals: 42.
	self assert: history last value equals: 43
]

{ #category : #tests }
GTSpotterTest >> testInitialText [
	
	self flag: 'is this instvar still required ? we have context'.
	self assert: spotter getText isEmptyOrNil.
	self assert: spotter text isEmptyOrNil.

]

{ #category : #tests }
GTSpotterTest >> testInstanceCreation [

	self assert: spotter notNil.
	self assertEmpty: spotter steps.
	self deny: spotter hasMultipleSteps
]

{ #category : #tests }
GTSpotterTest >> testInstanceCreationSmokeTest [

	self shouldnt: [ spotter := GTMockSpotter new ] raise: Error.

]

{ #category : #tests }
GTSpotterTest >> testNewSpotter [
	| step1 |

	self assert: spotter notNil.
	self assert: spotter hasSteps not.
	self assertEmpty: spotter steps.
	self deny: spotter hasMultipleSteps.

	self shouldnt: [ step1 := spotter currentStep ] raise: Error.

	self
		flag:
			'#currentStep is not always cached in spotter - especially when it is a new/empty spotter. In that case #currentStep will always return a new instance of a step. The implementation would be more polymorphic, oo-style, simpler and with much less ifTrue/ifFalse if the initial-state were regarded as a step as well (as an inst-var not in the inst-var-list of #steps). Maybe use a separate type to distinguish? GTSpotterInitialStep ... '.
	"self assert: step1 == spotter currentStep"

	self assert: step1 notNil.
	self assert: spotter hasSteps not.
	self assertEmpty: spotter steps
]
