"
Tests for MetacelloMonticelloLoader behavior, especially ignoreImage flag propagation through project cache and class-existence guards.
"
Class {
	#name : 'MetacelloMonticelloLoaderTest',
	#superclass : 'TestCase',
	#category : 'Metacello-Tests-Monticello',
	#package : 'Metacello-Tests',
	#tag : 'Monticello'
}

{ #category : 'tests' }
MetacelloMonticelloLoaderTest >> testCacheBypassWithIgnoreImage [
	| loader spec engine1 engine2 project1 |
	
	loader := MetacelloMonticelloLoader new.
	spec := MetacelloProjectSpec new
		className: 'BaselineOfTestToLock2';
		yourself.

	engine1 := MetacelloScriptEngine new.
	engine1 options at: #ignoreImage put: false.
	project1 := loader loadProject: spec engine: engine1. 
	
	"Without ignoreImage: cache returns same object"
	self assert: (loader loadProject: spec engine: engine1) == project1. 
	
	"With ignoreImage: cache bypassed, download attempted.
	Since no repository is configured, download fails with Error.
	If cache were NOT bypassed, this would return cached project silently."
	engine2 := MetacelloScriptEngine new.
	engine2 options at: #ignoreImage put: true.
	self should: [ loader loadProject: spec engine: engine2 ] raise: Error
]

{ #category : 'tests' }
MetacelloMonticelloLoaderTest >> testIgnoreImageDefaultIsFalse [
	| loader spec engine project1 project2 |
	
	loader := MetacelloMonticelloLoader new.
	spec := MetacelloProjectSpec new
		className: 'BaselineOfTestToLock2';
		yourself.

	engine := MetacelloScriptEngine new. 
	
	"engine has no ignoreImage key set"
	project1 := loader loadProject: spec engine: engine.
	project2 := loader loadProject: spec engine: engine.
	self assert: project2 identicalTo: project1
]
