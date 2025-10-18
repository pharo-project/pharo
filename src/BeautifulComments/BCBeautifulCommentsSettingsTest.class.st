"
This class test that two settings CaptureErrors and Rendering works as intended
"
Class {
	#name : 'BCBeautifulCommentsSettingsTest',
	#superclass : 'TestCase',
	#instVars : [
		'subject'
	],
	#category : 'BeautifulComments-Tests',
	#package : 'BeautifulComments',
	#tag : 'Tests'
}

{ #category : 'running' }
BCBeautifulCommentsSettingsTest >> setUp [
	super setUp.
	
	subject := BeautifulComments.
	BCFailingOnPurposeForTestBlock failOnPurpose: true
]

{ #category : 'running' }
BCBeautifulCommentsSettingsTest >> tearDown [
	
	BCFailingOnPurposeForTestBlock failOnPurpose: false.
	super tearDown
]

{ #category : 'tests' }
BCBeautifulCommentsSettingsTest >> testErrorCatch [
	"Chech that capture errors is working is working when true"
	| comment capturing|
	capturing := subject captureErrors.
	subject captureErrors: true.
	comment := subject  
			renderComment: BCFailingOnPurposeForTestBlock comment 
			of: self class..
	subject captureErrors: capturing 
]

{ #category : 'tests' }
BCBeautifulCommentsSettingsTest >> testNoErrorCatch [
	"Chech that capture errors is working is working when true"
	| capturing|
	capturing := subject captureErrors.
	subject captureErrors: false.
	self 
		should: [
			subject  
				renderComment: BCFailingOnPurposeForTestBlock comment 
				of: self class]
		raise:  BCFailingOnPurposeForTestError .
	subject captureErrors: capturing 
]

{ #category : 'tests' }
BCBeautifulCommentsSettingsTest >> testNotRenderingOfMyOwnComment [
	"Chech that rendering is working when false"
	| comment rendering|
	rendering := subject rendering.
	subject rendering: false.
	comment := subject  
			renderComment: self class comment 
			of: self class.
	self assert: comment isString.
	self deny: comment isText.
	self assert: (comment asString beginsWith: '# BCBeautifulCommentsSettingsTest').
	self assert: (comment asString includesSubstring: '## Description
This class test that two settings CaptureErrors and Rendering works as intended').
	subject rendering: rendering 
]

{ #category : 'tests' }
BCBeautifulCommentsSettingsTest >> testRenderingOfMyOwnComment [
	"Chech that rendering is working when true"
	| comment rendering|
	rendering := subject rendering.
	subject rendering: true.
	comment := subject  
			renderComment: self class comment 
			of: self class.
	self assert: comment isText.
	self assert: (comment asString beginsWith: 'BCBeautifulCommentsSettingsTest').
	self assert: (comment asString includesSubstring: 'This class test that two settings CaptureErrors and Rendering works as intended').
	subject rendering: rendering 
]

{ #category : 'tests' }
BCBeautifulCommentsSettingsTest >> testThereAreTwoSettings [
	"I am an approximation, just making sure the two class variables exist"
	self assert: subject classVarNames equals: #(CaptureErrors Rendering)
]
