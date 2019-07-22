"
I am a class that adds the completion to the settings
"
Class {
	#name : #TestCompletionController,
	#superclass : #NECController,
	#category : #'NECompletion-New'
}

{ #category : #private }
TestCompletionController >> contextClass [
	^ TestCompletionContext 
]
