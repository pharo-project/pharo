"
I am a behavior that can be added to debug points. 
I write the string in my text variable to the transcript window when executed.
"
Class {
	#name : 'TranscriptBehavior',
	#superclass : 'DebugPointMetaBehavior',
	#instVars : [
		'text'
	],
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'testing' }
TranscriptBehavior class >> isAbstract [

	^ false
]

{ #category : 'execution' }
TranscriptBehavior >> execute [
	self text trace.
	^true
]

{ #category : 'cleanup' }
TranscriptBehavior >> remove [ 
	"saving the text in case the behavior is added again later"
	self debugPoint properties at: #TranscriptText put: self text.
]

{ #category : 'accessing' }
TranscriptBehavior >> text [
	^text ifNil: [ '' ]
]

{ #category : 'accessing' }
TranscriptBehavior >> text: aString [
	text := aString.

]
