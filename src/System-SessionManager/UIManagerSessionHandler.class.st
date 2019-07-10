"
I am a session handler that will initialize the UIManager during startup.

This session handler makes the assumption that the current UIManager is a startup UI manager when its #startup: method gets called. Then, during startup he will install a Morphic UI manager.

During shutdown we put back a startup ui manager, so we can handle startup actions during next startup without depending in the UI. (However, we shouldnt need a UI manager during the first startup actions).
"
Class {
	#name : #UIManagerSessionHandler,
	#superclass : #AbstractSessionHandler,
	#classVars : [
		'UniqueInstance'
	],
	#category : #'System-SessionManager-Handlers'
}

{ #category : #'initialize-release' }
UIManagerSessionHandler class >> initialize [
	"has to be executed at the end of the user category"
	SessionManager default 
		register: self uniqueInstance
		inCategory: SessionManager default userCategory  
		atPriority: SmallInteger maxVal
]

{ #category : #accessing }
UIManagerSessionHandler class >> uniqueInstance [
	^ UniqueInstance ifNil: [ UniqueInstance := self new ]
]

{ #category : #accessing }
UIManagerSessionHandler >> handledId [
	^ self class name
]

{ #category : #handlers }
UIManagerSessionHandler >> shutdown: isImageQuitting [

	"Prepare the shutdown and the next startup"

	UIManager default: StartupUIManager new.

]

{ #category : #handlers }
UIManagerSessionHandler >> startup: isImageStarting [
	"Install the right UIManager"
	
	UIManager default: UIManager forCurrentSystemConfiguration.
		

]
