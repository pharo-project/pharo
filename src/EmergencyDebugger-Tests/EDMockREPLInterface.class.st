"
I am a null REPL interface for testing the Ed debugger.
I just implement a repl method that does nothing.
"
Class {
	#name : #EDMockREPLInterface,
	#superclass : #Object,
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #'command line' }
EDMockREPLInterface >> readEvalPrint [
]

{ #category : #'command line' }
EDMockREPLInterface >> terminateREPL [
]
