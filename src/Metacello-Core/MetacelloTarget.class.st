Class {
	#name : 'MetacelloTarget',
	#superclass : 'Object',
	#instVars : [
		'operator',
		'engine'
	],
	#classVars : [
		'RetryPackageResolution'
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
MetacelloTarget class >> retryPackageResolution [
	"if true, 
		errors during #retryingResolvePackageSpecReferences:gofer: are caught and 
		the resolution is retried 3 times. After the thrid time, a MetacelloPackageSpecResolutionError
		is thrown
	if false,
		an error during #retryingResolvePackageSpecReferences:gofer: will be passed,
		likely resulting in a walkback ... useful for debugging."

	^ RetryPackageResolution ifNil: [ RetryPackageResolution := true ]
]

{ #category : 'accessing' }
MetacelloTarget class >> retryPackageResolution: aBool [
    RetryPackageResolution := aBool
]

{ #category : 'accessing' }
MetacelloTarget >> engine [
	^ engine
]

{ #category : 'accessing' }
MetacelloTarget >> engine: aMetacelloScriptEngine [ 
	engine := aMetacelloScriptEngine
]

{ #category : 'accessing' }
MetacelloTarget >> operator [

	^ operator ifNil: [ #= ]
]

{ #category : 'accessing' }
MetacelloTarget >> operator: aSymbol [

	operator := aSymbol
]
