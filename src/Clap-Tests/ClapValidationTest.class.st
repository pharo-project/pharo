Class {
	#name : 'ClapValidationTest',
	#superclass : 'TestCase',
	#category : 'Clap-Tests-Unit',
	#package : 'Clap-Tests',
	#tag : 'Unit'
}

{ #category : 'tests' }
ClapValidationTest >> testErrorReportingWhenMissingNestedPositional [

	| spec context match validationReport errorString |
	spec := ClapCommandSpec id: #hello.
	spec addFlagWithPositional: #foo description: 'some foo'.
	spec addFlagWithPositional: #bar description: 'some bar'.
	context := ClapContext specification: spec.
	match := context
		         arguments: #( 'hello' '--foo' '--bar' );
		         match.
	validationReport := context allValidations.

	errorString := (ClapValidationErrorPrinter on: validationReport) printString.	
	self assert: (errorString includesSubstring: 'Usage: hello [--foo <foo-value>] [--bar <bar-value>]').
	self assert: (errorString includesSubstring: 'Missing positional value for foo-value').
	self assert: (errorString includesSubstring: 'Missing positional value for bar-value').
]

{ #category : 'tests' }
ClapValidationTest >> testErrorReportingWhenNotMatched [

	| match context validationReport errorString |
	context := ClapCommandLineExamples hello activateWith: #('hello' 'world' '--helpe').
	match := context match.
	validationReport := context allValidations.

	self assert: match isMatch.
	self deny: validationReport isSuccess.
	errorString := (ClapValidationErrorPrinter on: validationReport) printString.	
	self assert: (errorString includesSubstring: 'Usage: hello [--help]').
	self assert: (errorString includesSubstring: 'Unrecognized arguments: --helpe')
]

{ #category : 'tests' }
ClapValidationTest >> testNoErrorReportingWhenNestedPositionalNotInArguments [

	| spec context match validationReport errorString |
	spec := ClapCommandSpec id: #hello.
	spec addFlagWithPositional: #foo description: 'some foo'.
	context := ClapContext specification: spec.
	match := context
		         arguments: #( 'hello' );
		         match.
	validationReport := context allValidations.

	errorString := (ClapValidationErrorPrinter on: validationReport) printString.
	self assert: validationReport isSuccess.	
	self assert: errorString equals: ''
]
