"
I represent a Browser tool that is wanted by the browser according to its context.
I will be compared to existing tools to know if I need to be instantiated or if there is a tool that matches.
"
Class {
	#name : #ClyTargetTool,
	#superclass : #Object,
	#instVars : [
		#context,
		#activation,
		#matchedTool
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #initialization }
ClyTargetTool class >> forActivation: aClyTabActivationStrategyAnnotation context: aClyBrowserContext [

	^ self new
		forActivation: aClyTabActivationStrategyAnnotation context: aClyBrowserContext;
		yourself
]

{ #category : #accessing }
ClyTargetTool >> activation [

	^ activation
]

{ #category : #accessing }
ClyTargetTool >> context [
	^ context
]

{ #category : #matching }
ClyTargetTool >> createToolFor: browser [

	^ activation createToolFor: browser inContext: context
]

{ #category : #initialization }
ClyTargetTool >> forActivation: aClyTabActivationStrategyAnnotation context: aClyBrowserContext [

	activation := aClyTabActivationStrategyAnnotation.
	context := aClyBrowserContext
]

{ #category : #testing }
ClyTargetTool >> isMatched [

	^ matchedTool isNotNil
]

{ #category : #matching }
ClyTargetTool >> match: aListOfClyTool [

	aListOfClyTool
		detect: [ :tool | activation toolClass = tool class
			and: [ tool isValidInContext: self context ] ]
		ifFound: [ :tool | matchedTool := tool ]
		ifNone: [ "ignore" ]
]

{ #category : #accessing }
ClyTargetTool >> matchedTool [

	^ matchedTool
]
