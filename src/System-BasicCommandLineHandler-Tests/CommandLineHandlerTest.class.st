Class {
	#name : #CommandLineHandlerTest,
	#superclass : #TestCase,
	#category : #'System-BasicCommandLineHandler-Tests'
}

{ #category : #utilities }
CommandLineHandlerTest >> argumentsWith: aCollection [
	^ CommandLineArguments withArguments: aCollection
]

{ #category : #tests }
CommandLineHandlerTest >> testBasicCommandlineHandler [
	
	| args |
	args := self argumentsWith: #().
	"BasicCommandLineHandler gets activated by default on image startup, so there is no need to activate it in a nother case"
	self deny: (BasicCommandLineHandler isResponsibleFor: args).
	"The BasicCommandLineHandler should always delegate to the PharoCommandLineHandler if it is present"
	self 
		assert: BasicCommandLineHandler new selectedHandler
		equals: PharoCommandLineHandler
]

{ #category : #tests }
CommandLineHandlerTest >> testResponsibilityDefault [
	| args |
	args := self argumentsWith: #('--help').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self deny: (EvaluateCommandLineHandler isResponsibleFor: args).
]

{ #category : #tests }
CommandLineHandlerTest >> testResponsibilityEval [
	| args |
	args := self argumentsWith: #('eval' '1+2').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self assert: (EvaluateCommandLineHandler isResponsibleFor: args).
	
	args := self argumentsWith: #('-e' '1+2').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self assert: (EvaluateCommandLineHandler isResponsibleFor: args).
	
	args := self argumentsWith: #('--evaluate' '1+2').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self assert: (EvaluateCommandLineHandler isResponsibleFor: args).
]

{ #category : #tests }
CommandLineHandlerTest >> testResponsibilitySt [
	| args |
	args := self argumentsWith: #('/foo/bar/myScript.st').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self assert: (STCommandLineHandler isResponsibleFor: args).
	self deny: (EvaluateCommandLineHandler isResponsibleFor: args).
	
	args := self argumentsWith: #('st' '/foo/bar/myScript.st').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self assert: (STCommandLineHandler isResponsibleFor: args).
	self deny: (EvaluateCommandLineHandler isResponsibleFor: args).
]

{ #category : #tests }
CommandLineHandlerTest >> testSelectHandlersCodeLoader [
	| args handlers |
	args := self argumentsWith: #('/foo/bar/myScript.st').
	handlers := CommandLineHandler selectHandlersFor: args.
	self assert: handlers first = STCommandLineHandler.
	
	args := self argumentsWith: #('/foo/bar/myScript.st' '--verbose').
	handlers := CommandLineHandler selectHandlersFor: args.
	self assert: handlers first = STCommandLineHandler.
]
