"
I am the command allowing one to rerun a subparts of the results of a test-run.
"
Class {
	#name : #DTRerunCommand,
	#superclass : #DTResultCommand,
	#category : #'DrTests-TestsRunner'
}

{ #category : #hooks }
DTRerunCommand >> execute [
	self drTest runPluginFor: (self plugin buildReRunConfigurationFrom: self drTest)
]

{ #category : #hooks }
DTRerunCommand >> initialize [
	super initialize.
	self name: 'Re-run tests'
]
