"
I represent a package for benchmarking.
I mainly give access to methds in the package.
I am required because packages and methods are not polymorphic.
"
Class {
	#name : 'CoBenchmarkPackage',
	#superclass : 'Object',
	#instVars : [
		'package'
	],
	#category : 'HeuristicCompletion-Model-Benchmarks',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Benchmarks'
}

{ #category : 'instance creation' }
CoBenchmarkPackage class >> on: aPackage [

	^ self new
		package: aPackage;
		yourself
]

{ #category : 'enumerating' }
CoBenchmarkPackage >> methodsDo: aBlockClosure [

	package definedClasses do: [ :class | class isTrait ifFalse: [ (package definedMethodsForClass: class) do: aBlockClosure ] ]
]

{ #category : 'accessing' }
CoBenchmarkPackage >> package: aPackage [

	package := aPackage
]

{ #category : 'printing' }
CoBenchmarkPackage >> printOn: aStream [

	aStream print: package name
]
