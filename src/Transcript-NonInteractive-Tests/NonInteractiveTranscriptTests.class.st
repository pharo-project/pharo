"
I am NonInteractiveTranscriptTests
"
Class {
	#name : #NonInteractiveTranscriptTests,
	#superclass : #TestCase,
	#category : #'Transcript-NonInteractive-Tests'
}

{ #category : #tests }
NonInteractiveTranscriptTests >> testAppend [
	| log transcript |
	log := 'test.log' asFileReference.
	log ensureDelete.
	transcript := NonInteractiveTranscript onFileNamed: log basename.
	transcript
		show: 'One'; cr; 
		show: 'Two'; cr; 
		close.
	transcript := NonInteractiveTranscript onFileNamed: log basename.
	transcript
		show: 'Three'; cr; 
		close.
	self assert: log contents lines equals: #('One' 'Two' 'Three').
	log ensureDelete
]

{ #category : #tests }
NonInteractiveTranscriptTests >> testSimple [
	| log transcript |
	log := 'test.log' asFileReference.
	log ensureDelete.
	transcript := NonInteractiveTranscript onFileNamed: log basename.
	transcript
		show: 'One'; cr; 
		show: 'Two'; cr; 
		show: 'Three'; cr; 
		close.
	self assert: log contents lines equals: #('One' 'Two' 'Three').
	log ensureDelete
]

{ #category : #tests }
NonInteractiveTranscriptTests >> testStderr [
	NonInteractiveTranscript stderr << 'Pharo writing to the standard error'; cr; flush
]

{ #category : #tests }
NonInteractiveTranscriptTests >> testStdout [
	NonInteractiveTranscript stdout << 'Pharo writing to the standard output'; cr; flush
]
