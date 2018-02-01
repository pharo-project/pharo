"
A NECControllerTest is a test class for testing the behavior of NECController
"
Class {
	#name : #NECControllerTest,
	#superclass : #TestCase,
	#instVars : [
		'controller'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #running }
NECControllerTest >> setUp [
	"Setting up code for NECControllerTest"

	super setUp.
	controller := NECController new.
]

{ #category : #running }
NECControllerTest >> tearDown [
	"Tearing down code for NECControllerTest"

	controller := nil.
	super tearDown
]

{ #category : #'tests-keyboard' }
NECControllerTest >> testSmartNeedExtraRemoveInForOppositeAt [

	self assert: (controller smartNeedExtraRemoveIn: '()' for: $( opposite:$) at:1).
	self deny: (controller smartNeedExtraRemoveIn: '(()' for: $( opposite:$) at:2).
	self assert: (controller smartNeedExtraRemoveIn: '(1()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(1(1())' for: $( opposite:$) at:5).
	self assert: (controller smartNeedExtraRemoveIn: '((1)1())))' for: $( opposite:$) at:6).
	self deny: (controller smartNeedExtraRemoveIn: '()(()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(foobar()' for: $( opposite:$) at:8).
]
