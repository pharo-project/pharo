"
tests related to environment
"
Class {
	#name : #OCEnvironmentScopeTest,
	#superclass : #TestCase,
	#instVars : [
		'oldSystemAnnouncer'
	],
	#category : #'OpalCompiler-Tests-Semantic'
}

{ #category : #'tests - environment' }
OCEnvironmentScopeTest >> testCompileWithEnvironment [
	| environment method return | 
	environment := SystemDictionary new.
	environment at: #MyClass put: Point.
	method := Smalltalk compiler 
					environment: environment;
					compile: 'tt ^MyClass'.
	return := method valueWithReceiver: nil arguments: #().
	self assert: return equals: Point.
	"I d not know about anything else"
	method := Smalltalk compiler 
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

{ #category : #'tests - environment' }
OCEnvironmentScopeTest >> testCreateEnvironmentScope [
	| new |
	new := OCEnvironmentScope for: Smalltalk globals.
	self assert: ((new lookupVar: #Object) isKindOf: OCLiteralVariable).
]
