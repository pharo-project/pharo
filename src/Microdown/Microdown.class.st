"
I'm a kind of facade to steer Microdown services.
"
Class {
	#name : #Microdown,
	#superclass : #Object,
	#instVars : [
		'parser',
		'resolverRoot',
		'file'
	],
	#category : #'Microdown-Core'
}

{ #category : #parsing }
Microdown >> builder [

	^ parser builder
]

{ #category : #parsing }
Microdown >> initialize [ 
	super initialize.
	parser := MicroDownParser new.
	 
]

{ #category : #parsing }
Microdown >> parse: aStreamOrString [

	^ parser parse: aStreamOrString
]

{ #category : #parsing }
Microdown >> resolvedTreeFrom: aStreamOrString [

	| micTree |
	micTree := parser parse: aStreamOrString.
	self resolver
		microdown: self; 
		originFile: file;
		resolutionSource: self resolverRoot ;
		visit: micTree.
	^ micTree
	
]

{ #category : #resolving }
Microdown >> resolvedTreeFromFile: aFileReference [
	"This method is needed because when we resolved path we often need to know in which file a given figure or inputFile is written."
	
	file := aFileReference. 
	^ self resolvedTreeFrom: file contents
]

{ #category : #parsing }
Microdown >> resolver [
	^ MicPathResolver new resolutionSource: self resolverRoot; yourself
]

{ #category : #parsing }
Microdown >> resolverRoot [
	
	^ resolverRoot ifNil: [ resolverRoot := FileSystem workingDirectory ]
]

{ #category : #parsing }
Microdown >> resolverRoot: root [
	
	resolverRoot := root
]
