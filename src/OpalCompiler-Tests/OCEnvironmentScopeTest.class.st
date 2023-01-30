"
tests related to environment
"
Class {
	#name : #OCEnvironmentScopeTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Semantic'
}

{ #category : #'tests - environment' }
OCEnvironmentScopeTest >> testCompileWithEnvironment [
	"testing compiling with environments other then Smalltalk globals. Note: the class in the environment has to point to the environment. And we have to compile for a class in the environment"
	| environment method return | 
	environment := SystemDictionary new.
	environment at: #MyClass put: Point copy.
	environment at: #MyClass2 put: Association.
	method := OpalCompiler new
					class: (environment at: #MyClass);
					environment: environment;
					compile: 'tt ^MyClass'.
	return := method valueWithReceiver: nil arguments: #().
	self assert: return name equals: 'Point'.
	method := OpalCompiler new
					class: (environment at: #MyClass);
					environment: environment;
					compile: 'tt ^MyClass2'.
	return := method valueWithReceiver: nil arguments: #().
	self assert: return name equals: 'Association'.
	"I d not know about anything else"
	method := OpalCompiler new
					class: (environment at: #MyClass);
					environment: environment;
					compile: 'tt ^Object'.
	return := method valueWithReceiver: nil arguments: #().
	self assert: return equals: nil.
]

{ #category : #'test - production - environment' }
OCEnvironmentScopeTest >> testCompileWithProductionEnvironment [
	| environment method return | 
	environment := {#CompiledMethod -> MyCompiledMethodForTesting} asDictionary.
	method := Smalltalk compiler 
					productionEnvironment: environment;
					compile: 'tt ^1+2'.
	
	self assert: method class equals: MyCompiledMethodForTesting.			
	return := method valueWithReceiver: nil arguments: #().
	self assert: return equals: 3.
]
