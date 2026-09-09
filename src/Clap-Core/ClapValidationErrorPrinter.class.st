"
I pretty print an informative message explaining errors found according to the command specification.
"
Class {
	#name : 'ClapValidationErrorPrinter',
	#superclass : 'Object',
	#instVars : [
		'report',
		'writeStream'
	],
	#category : 'Clap-Core-Validation',
	#package : 'Clap-Core',
	#tag : 'Validation'
}

{ #category : 'initialization' }
ClapValidationErrorPrinter class >> on: aClapValidationReport [ 

	^ self new
		validationReport: aClapValidationReport;
		yourself
]

{ #category : 'printing' }
ClapValidationErrorPrinter >> printCommandHelp [

	(ClapDocumenter on: writeStream) 
		explain: report commandSpecification
]

{ #category : 'printing' }
ClapValidationErrorPrinter >> printDelimiterLine [

	writeStream
		<< '--------------------';
		cr
]

{ #category : 'printing' }
ClapValidationErrorPrinter >> printOn: aStream [

	report isSuccess ifTrue: [ ^ '' ].

	writeStream := aStream.
	report problems do: [ :problem | 
		aStream
			nextPutAll: 'Error: ';
			nextPutAll: problem description;
			cr ].
	self printDelimiterLine.
	self printCommandHelp
]

{ #category : 'printing' }
ClapValidationErrorPrinter >> printString [

	^ String streamContents: [ :stream | self printOn: stream ]
]

{ #category : 'initialization' }
ClapValidationErrorPrinter >> validationReport: aClapValidationReport [ 
	
	report := aClapValidationReport
]
