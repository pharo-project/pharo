"
I provide control over halts messages and breakpoints for activating and deactivating them.
I also tell if a program node has active breakpoints or halts on it.
"
Class {
	#name : #StHaltAndBreakpointController,
	#superclass : #Object,
	#classInstVars : [
		'tracer'
	],
	#category : #'NewTools-Debugger-Breakpoints-Tools-Model'
}

{ #category : #Helpers }
StHaltAndBreakpointController class >> printSkipTextFor: aProgramNode [
	^ aProgramNode methodNode source asText
		makeBoldFrom: aProgramNode start to: aProgramNode stop;
		yourself
]

{ #category : #API }
StHaltAndBreakpointController class >> skipHaltNode: aProgramNode [
	self trace: (self printSkipTextFor: aProgramNode)
]

{ #category : #Helpers }
StHaltAndBreakpointController class >> trace: aTextOrString [
	self tracer crTrace: aTextOrString
]

{ #category : #Helpers }
StHaltAndBreakpointController class >> tracer [
	^ tracer ifNil: [ tracer := Transcript ]
]

{ #category : #Helpers }
StHaltAndBreakpointController class >> tracer: aTracer [
	tracer := aTracer
]

{ #category : #API }
StHaltAndBreakpointController >> areHaltsAndBreakpoinsEnabledFor: aProgramNode [
	^ aProgramNode enabledForHaltOrBreakpoint
]

{ #category : #control }
StHaltAndBreakpointController >> disableBreakpointNode: aProgramNode [
	aProgramNode allBreakpointLinks do: [ :link | link condition: [false]; disable ].
	self skipNode: aProgramNode
]

{ #category : #links }
StHaltAndBreakpointController >> disableHaltLink: control [
	^ MetaLink new
		metaObject: self class;
		selector: #skipHaltNode:;
		arguments: #(node);
		control: control;
		yourself
]

{ #category : #control }
StHaltAndBreakpointController >> disableHaltNode: aMessageNode [
	self skipNode: aMessageNode
]

{ #category : #API }
StHaltAndBreakpointController >> disableHaltOrBreakpointFor: aProgramNode [
	aProgramNode isHaltNode
		ifTrue: [ self disableHaltNode: aProgramNode ].
	aProgramNode hasBreakpoint
		ifTrue: [ self disableBreakpointNode: aProgramNode ]
]

{ #category : #API }
StHaltAndBreakpointController >> disableObjectCentricBreakpoint: anObjectCentricBreakpoint [
	anObjectCentricBreakpoint targetInstance ifNil:[^self].
	anObjectCentricBreakpoint link condition: [false]; disable
]

{ #category : #control }
StHaltAndBreakpointController >> enableBreakpointNode: aProgramNode [
	aProgramNode allBreakpointLinks do: [ :link | link condition: [true]; enable ].
	self removeSkipLinksFor: aProgramNode
]

{ #category : #control }
StHaltAndBreakpointController >> enableHaltNode: aMessageNode [
	self removeSkipLinksFor: aMessageNode
]

{ #category : #API }
StHaltAndBreakpointController >> enableHaltOrBreakpointFor: aProgramNode [
	aProgramNode isHaltNode
		ifTrue: [ self enableHaltNode: aProgramNode ].
	aProgramNode hasBreakpoint
		ifTrue: [ self enableBreakpointNode: aProgramNode ].
	aProgramNode enableForHaltOrBreakpoint: true
]

{ #category : #API }
StHaltAndBreakpointController >> enableObjectCentricBreakpoint: anObjectCentricBreakpoint [
	anObjectCentricBreakpoint targetInstance ifNil: [ ^ self ].
	anObjectCentricBreakpoint link condition: [true]; enable
]

{ #category : #links }
StHaltAndBreakpointController >> installSkipLinkOn: aMessageNode [
	| control |
	control := aMessageNode isReturn
		ifTrue: [ #before ]
		ifFalse: [ #instead ].
	aMessageNode link: (self disableHaltLink: control)
]

{ #category : #API }
StHaltAndBreakpointController >> isObjectCentricBreakpointEnabled: anObjectCentricBreakpoint [
	anObjectCentricBreakpoint targetInstance ifNil: [ ^ false ].
	^ anObjectCentricBreakpoint node enabledForHaltOrBreakpoint
]

{ #category : #links }
StHaltAndBreakpointController >> removeSkipLinksFor: aNode [
	(aNode links
		select:
			[ :link | link control = #instead and: [ link metaObject == self class ] ])
		do: [ :link | link uninstall ]
]

{ #category : #control }
StHaltAndBreakpointController >> skipNode: aNode [
	(self areHaltsAndBreakpoinsEnabledFor: aNode)
		ifFalse: [ ^ self ].
	aNode enableForHaltOrBreakpoint: false.
	self installSkipLinkOn: aNode
]
