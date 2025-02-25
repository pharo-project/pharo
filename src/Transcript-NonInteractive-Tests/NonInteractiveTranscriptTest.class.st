"
I am NonInteractiveTranscriptTests
"
Class {
	#name : 'NonInteractiveTranscriptTest',
	#superclass : 'TestCase',
	#category : 'Transcript-NonInteractive-Tests',
	#package : 'Transcript-NonInteractive-Tests'
}

{ #category : 'tests' }
NonInteractiveTranscriptTest >> testAppend [
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

{ #category : 'tests' }
NonInteractiveTranscriptTest >> testSimple [

	| log transcript |
	log := 'test.log' asFileReference.
	log ensureDelete.
	transcript := NonInteractiveTranscript onFileNamed: log basename.
	transcript
		show: 'One';
		cr;
		show: 'Two';
		cr;
		show: 'Three';
		cr;
		close.
	self assert: log contents lines equals: #( 'One' 'Two' 'Three' ).
	log ensureDelete 
]

{ #category : 'tests' }
NonInteractiveTranscriptTest >> testStderr [
	NonInteractiveTranscript stderr << 'Pharo writing to the standard error'; cr; flush
]

{ #category : 'tests' }
NonInteractiveTranscriptTest >> testStdout [
	NonInteractiveTranscript stdout << 'Pharo writing to the standard output'; cr; flush
]
