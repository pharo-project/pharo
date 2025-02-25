"
A MockMessageSendTest is a test class for testing the behavior of MockMessageSend
"
Class {
	#name : 'MockMessageSendTest',
	#superclass : 'TestCase',
	#instVars : [
		'mockMessage',
		'testStream'
	],
	#category : 'SUnit-MockObjects-Tests-Core',
	#package : 'SUnit-MockObjects-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
MockMessageSendTest >> setUp [

	super setUp.
	testStream := WriteStream on: ''
	
]

{ #category : 'running' }
MockMessageSendTest >> testBinaryMessagePrintOn [
	mockMessage := MockMessageSend on: #binaryMsg: with: #(true) do: [ ].
	
	mockMessage printOn: testStream.
	
	self assert: (testStream contents includesSubstring: 'binaryMsg: true')
]

{ #category : 'running' }
MockMessageSendTest >> testKeywordMessageArrayPrintOn [
	mockMessage := MockMessageSend on: #keyWord:msg: with: #('hello' #(1 2 3)) do: [ ].
	
	mockMessage printOn: testStream.
	
	self assert: (testStream contents includesSubstring: 'keyWord:msg: ( ''hello'', #(1 2 3) )')
]

{ #category : 'running' }
MockMessageSendTest >> testKeywordMessagePrintOn [
	mockMessage := MockMessageSend on: #keyWord:msg: with: #('hello' 'world') do: [ ].
	
	mockMessage printOn: testStream.
	
	self assert: (testStream contents includesSubstring: 'keyWord:msg: ( ''hello'', ''world'' )')
]

{ #category : 'running' }
MockMessageSendTest >> testUnaryMessagePrintOn [
	mockMessage := MockMessageSend on: #unaryMsg with: #() do: [ ].
	
	mockMessage printOn: testStream.
	
	self assert: (testStream contents includesSubstring: 'unaryMsg')
]
