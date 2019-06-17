"
A ResumeDebugAction resumes the execution of the interrupted process and closes the debugger.

"
Class {
	#name : #ResumeDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
ResumeDebugAction class >> actionType [
	<debuggingAction>
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
ResumeDebugAction >> defaultKeymap [

	^ PharoShortcuts current resumeExecutionShortcut
]

{ #category : #accessing }
ResumeDebugAction >> defaultLabel [

	^ 'Proceed'
]

{ #category : #accessing }
ResumeDebugAction >> defaultOrder [

	^ 5
]

{ #category : #accessing }
ResumeDebugAction >> enabled [

	^ self interruptedContextIsPostMortem not
]

{ #category : #actions }
ResumeDebugAction >> executeAction [

	self session 
		resume;
		clear.
	self debugger close. 
]

{ #category : #accessing }
ResumeDebugAction >> help [
	^ 'Relinquish debugger control and proceed execution from the current point of debugger control.'
]

{ #category : #accessing }
ResumeDebugAction >> id [

	^ #resume
]

{ #category : #initialization }
ResumeDebugAction >> initialize [

	super initialize.
	"No need to update the debgger as it will anyway be closed."
	self needsUpdate: false 
]

{ #category : #private }
ResumeDebugAction >> interruptedContextIsPostMortem [

	^ (self session isContextPostMortem: self interruptedContext) 
]
