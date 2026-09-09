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
