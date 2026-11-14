"
I am a build target of Metacello, like a target of a makefile: one step of what Metacello does with a project, holding that step's behavior. A target visits specs or directives, with the engine that runs the script; the engine keeps what the steps share (the resolution, the options, the loader).

A load is a chain of targets, each one building on the one before:
- MetacelloResolveTarget chooses the version of every project the load reaches (a resolution);
- MetacelloRecordTarget walks a version spec and records the load directives from the resolution: which packages and projects to load, in which order;
- MetacelloFetchTarget fetches what those directives need;
- MetacelloLoadTarget loads them, installs the baselines the resolver only read, and registers every project loaded.

A resolve script runs only the first; the version spec runs the others (see MetacelloVersionSpec>>#recordRequiredFromArray:withEngine:, #fetchRequiredFromArray:withEngine: and #loadRequiredFromArray:withEngine:).
"
Class {
	#name : 'MetacelloTarget',
	#superclass : 'Object',
	#instVars : [
		'engine'
	],
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'utilities' }
MetacelloTarget class >> nameComponentsFrom: aVersionName [
	| ar |
	ar := (aVersionName last isDigit and: [ (aVersionName indexOf: $.) > 0 ])ifFalse: [Array with: aVersionName with: '' with: 0 with: aVersionName with: self]
		ifTrue: [ | vrsn str |
			str := ((aVersionName copyAfterLast: $-) copyAfterLast: $.).
			vrsn := str isEmpty
				ifTrue: [0]
				ifFalse: [str asInteger].
			Array
				with: (aVersionName copyUpToLast:  $-)								"base pkg name"
				with: ((aVersionName copyAfterLast: $-) copyUpTo: $.)				"user"
				with:  vrsn "version"
				with: aVersionName
				with: self ].
	^ar
]

{ #category : 'accessing' }
MetacelloTarget class >> on: anEngine [

	^ self new
		  engine: anEngine;
		  yourself
]

{ #category : 'accessing' }
MetacelloTarget >> engine [
	^ engine
]

{ #category : 'accessing' }
MetacelloTarget >> engine: aMetacelloScriptEngine [ 
	engine := aMetacelloScriptEngine
]
