"
this verify that there is at least an tabulation or 3 spaces when you write in the method body.
"
Class {
	#name : 'ReIndentationMethodBodyRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'accessing' }
ReIndentationMethodBodyRule class >> group [ 

	^ 'Formatting'
]

{ #category : 'accessing' }
ReIndentationMethodBodyRule class >> ruleName [ 

	^ 'make at least an indentation when you write in the method body'
]

{ #category : 'helpers' }
ReIndentationMethodBodyRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode methodNode body
			interval: (Interval from: aNode methodNode body start to: aNode stop)
]

{ #category : 'running' }
ReIndentationMethodBodyRule >> basicCheck: anEntity [

	| endOfHeader position found |
	
	found := false.
	endOfHeader:= anEntity methodNode arguments
		ifEmpty: [ anEntity methodNode selector keywords first size + 1 ]
		ifNotEmpty: [
			| lastArgument |
			lastArgument := anEntity methodNode arguments last.
			lastArgument stop ].
	
	position := anEntity methodNode body stop.
	
	anEntity methodNode comments notEmpty ifTrue:[ 
		endOfHeader := endOfHeader max: (anEntity methodNode comments first) stop ].
	
	anEntity methodNode body statements do: [ :statement | 
		found := found or: [ (position + 1 ~= statement start) ifFalse: [ position + 3 ~= statement start ] ifTrue: [ true ] ].
		position := statement stop ].
	

   	^ found "aNode isSequence and: [ aNode notNil and: [ aNode start + 3 ~= aNode start value] ]"
]
