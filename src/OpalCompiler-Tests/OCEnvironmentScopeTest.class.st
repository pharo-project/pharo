"
tests related to environment
"
Class {
	#name : 'OCEnvironmentScopeTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'tests - environment' }
OCEnvironmentScopeTest >> testCompileWithEnvironment [
	"testing compiling with environments other then Smalltalk globals. Note: the class in the environment has to point to the environment. And we have to compile for a class in the environment"


	| environment method return |
	environment := SystemDictionary new.
	environment at: #MyClass put: Point copy.
	(environment at: #MyClass) environment: environment.
	method := OpalCompiler new
					environment: environment;
					class: (environment at: #MyClass);
					compile: 'tt ^MyClass'.
	return := method valueWithReceiver: nil arguments: #().
	self assert: return name equals: 'Point'.
	"I d not know about anything else"
	method := OpalCompiler new
					environment: environment;
					class: (environment at: #MyClass);
					permitFaulty: true;
					compile: 'tt ^Object'.
	self should: [ method valueWithReceiver: nil arguments: #() ] raise: UndeclaredVariableRead
]

{ #category : 'test - production - environment' }
OCEnvironmentScopeTest >> testCompileWithProductionEnvironment [
	| environment method return |
	environment := {#CompiledMethod -> MyCompiledMethodForTesting} asDictionary.
	method := Smalltalk compiler
					productionEnvironment: environment;
					compile: 'tt ^1+2'.

	self assert: method class equals: MyCompiledMethodForTesting.
	return := method valueWithReceiver: nil arguments: #().
	self assert: return equals: 3
]
