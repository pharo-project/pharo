"
""super"" only makes sense when used as the receiver of a message.

	super doSomething.
	
For every other use, it is just the same as ""self"".

"
Class {
	#name : 'ReSuperWithoutSend',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'accessing' }
ReSuperWithoutSend class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReSuperWithoutSend class >> ruleName [
	^ 'super makes only sense as the receiver of a message'
]

{ #category : 'running' }
ReSuperWithoutSend >> basicCheck: aNode [
	aNode isVariable ifFalse: [ ^ false ].
	aNode isSuperVariable ifFalse: [ ^ false ].
	aNode parent isMessage ifTrue: [
		"if we are the receiver, everything is ok"
		^aNode parent receiver ~= aNode
	].
	^true
]
