"
tests related to environment
"
Class {
	#name : #OCEnvironmentScopeTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Semantic'
}

{ #category : #tests }
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

{ #category : #tests }
OCEnvironmentScopeTest >> testCreateEnvironmentScope [
	| new |
	new := OCEnvironmentScope for: Smalltalk globals.
	self assert: ((new lookupVar: #Object) isKindOf: OCLiteralVariable).
]
