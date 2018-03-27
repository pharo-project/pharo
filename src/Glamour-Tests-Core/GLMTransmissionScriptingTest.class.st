Class {
	#name : #GLMTransmissionScriptingTest,
	#superclass : #TestCase,
	#instVars : [
		'browser'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #running }
GLMTransmissionScriptingTest >> setUp [
	super setUp.
	browser := GLMExplicitBrowser new.
	browser addPane: (GLMPane new name: #one).
	browser addPane: (GLMPane new name: #two).
	browser addPane: (GLMPane new name: #three).

]

{ #category : #tests }
GLMTransmissionScriptingTest >> testFrom [
	browser transmit from: #one.
	self assert: browser transmissions first origins first pane equals: browser panes first.
	self assert: browser transmissions first origins first name equals: GLMPortIdentifier defaultOriginPortName
]

{ #category : #tests }
GLMTransmissionScriptingTest >> testFromPort [
	browser transmit from: #one port: #port.
	self assert: browser transmissions first origins first pane equals: browser panes first.
	self assert: browser transmissions first origins first name equals: #port
]

{ #category : #tests }
GLMTransmissionScriptingTest >> testTo [
	browser transmit to: #one.
	self assert: browser transmissions first destination pane equals: browser panes first.
	self assert: browser transmissions first destination name equals: GLMPortIdentifier defaultTargetPortName
]

{ #category : #tests }
GLMTransmissionScriptingTest >> testToPort [
	browser transmit to: #one port: #port.
	self assert: browser transmissions first destination pane equals: browser panes first.
	self assert: browser transmissions first destination name equals: #port
]

{ #category : #tests }
GLMTransmissionScriptingTest >> testToWatcher [
	browser transmit toWatcher.
	self assert: browser transmissions first destination pane equals: browser watcherPane
]
