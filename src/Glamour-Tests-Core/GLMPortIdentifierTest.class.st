Class {
	#name : #GLMPortIdentifierTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMPortIdentifierTest >> testAsOriginIdentifier [
	| id |
	id := GLMPortIdentifier new.
	self assert: id asGlamourOriginIdentifier equals: id
]

{ #category : #tests }
GLMPortIdentifierTest >> testAsTargetIdentifier [
	| id |
	id := GLMPortIdentifier new.
	self assert: id asGlamourTargetIdentifier equals: id
]

{ #category : #tests }
GLMPortIdentifierTest >> testAssociationAsOriginIdentifier [
	self assert: ((#pane -> #port) asGlamourOriginIdentifier isKindOf: GLMPortIdentifier).
	self assert: (#pane -> #port) asGlamourOriginIdentifier paneName equals: #pane.
	self assert: (#pane -> #port) asGlamourOriginIdentifier portName equals: #port
]

{ #category : #tests }
GLMPortIdentifierTest >> testAssociationAsTargetIdentifier [
	self assert: ((#pane -> #port) asGlamourTargetIdentifier isKindOf: GLMPortIdentifier).
	self assert: (#pane -> #port) asGlamourTargetIdentifier paneName equals: #pane.
	self assert: (#pane -> #port) asGlamourTargetIdentifier portName equals: #port
]

{ #category : #tests }
GLMPortIdentifierTest >> testSymbolAsOriginIdentifier [
	self assert: (#pane asGlamourOriginIdentifier isKindOf: GLMPortIdentifier).
	self assert: #pane asGlamourOriginIdentifier paneName equals: #pane.
	self assert: #pane asGlamourOriginIdentifier portName equals: #selection
]

{ #category : #tests }
GLMPortIdentifierTest >> testSymbolAsTargetIdentifier [
	self assert: (#pane asGlamourTargetIdentifier isKindOf: GLMPortIdentifier).
	self assert: #pane asGlamourTargetIdentifier paneName equals: #pane.
	self assert: #pane asGlamourTargetIdentifier portName equals: #entity
]
