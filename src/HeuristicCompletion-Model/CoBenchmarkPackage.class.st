Class {
	#name : #CoBenchmarkPackage,
	#superclass : #Object,
	#instVars : [
		'package'
	],
	#category : #'HeuristicCompletion-Model-Benchmarks'
}

{ #category : #'instance creation' }
CoBenchmarkPackage class >> on: aRPackage [ 
	
	^ self new
		package: aRPackage;
		yourself
]

{ #category : #enumerating }
CoBenchmarkPackage >> methodsDo: aBlockClosure [ 
	
	package definedClassesDo: [ :each | | class |
		class := self class environment at: each.
		class isTrait ifFalse: [
			(package definedMethodsForClass: class) do: aBlockClosure] ]
]

{ #category : #accessing }
CoBenchmarkPackage >> package: aRPackage [ 
	
	package := aRPackage
]

{ #category : #printing }
CoBenchmarkPackage >> printOn: aStream [

	aStream print: package name
]
