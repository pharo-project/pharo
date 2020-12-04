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
StHaltAndBreakpointController >> disableBreakpoint: aBreakpoint [

	aBreakpoint disable
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

{ #category : #API }
StHaltAndBreakpointController >> disableHaltNode: aMessageNode [
	self skipNode: aMessageNode.
	aMessageNode enableHalts: false
]

{ #category : #API }
StHaltAndBreakpointController >> disableHaltOrBreakpointFor: haltOrBreakpointItem [

	haltOrBreakpointItem disableInContext: self
]

{ #category : #API }
StHaltAndBreakpointController >> enableBreakpoint: aBreakpoint [

	aBreakpoint enable
]

{ #category : #API }
StHaltAndBreakpointController >> enableHaltNode: aMessageNode [
	self removeSkipLinksFor: aMessageNode.
	aMessageNode enableHalts: true
]

{ #category : #API }
StHaltAndBreakpointController >> enableHaltOrBreakpointFor: haltOrBreakpointItem [

	haltOrBreakpointItem enableInContext: self
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
StHaltAndBreakpointController >> isBreakpointEnabled: aBreakpoint [ 
	^aBreakpoint isEnabled
]

{ #category : #API }
StHaltAndBreakpointController >> isHaltEnabledFor: aProgramNode [
	^ aProgramNode enabledForHaltOrBreakpoint
]

{ #category : #API }
StHaltAndBreakpointController >> isHaltNodeEnabled: aRBMessageNode [ 
	^aRBMessageNode enabledForHaltOrBreakpoint
]

{ #category : #API }
StHaltAndBreakpointController >> isHaltOrBreakpointEnabled: aStBreakpointInspectionItem [ 
	^aStBreakpointInspectionItem isEnabledInContext: self
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
	(self isHaltEnabledFor: aNode)
		ifFalse: [ ^ self ].
	aNode enableHalts: false.
	self installSkipLinkOn: aNode
]
