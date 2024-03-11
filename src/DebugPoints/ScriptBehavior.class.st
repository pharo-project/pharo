"
I am a script behavior that can be added to debug points. 
I execute arbitrary code that is saved in my script variable.
"
Class {
	#name : 'ScriptBehavior',
	#superclass : 'DebugPointBehavior',
	#instVars : [
		'script'
	],
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'accessing' }
ScriptBehavior >> context [
	^self getArgument: #context
]

{ #category : 'accessing' }
ScriptBehavior >> evaluate: expression withContext: aContext [

	^ Smalltalk compiler
		  source: expression;
		  context: aContext;
		  receiver: aContext receiver;
		  bindings: { (#thisContext -> aContext) };
		  evaluate
]

{ #category : 'execution' }
ScriptBehavior >> execute [

	self script ifNil: [ ^ true ].
	self evaluate: self script withContext: self context.
	^ true
]

{ #category : 'cleanup' }
ScriptBehavior >> remove [ 
	"saving the script in case the script behavior is added again later"
	self debugPoint properties at: #Script put: self script.
]

{ #category : 'accessing' }
ScriptBehavior >> script [
	^script ifNil:['']
]

{ #category : 'accessing' }
ScriptBehavior >> script: aScript [
	script := aScript 
]
