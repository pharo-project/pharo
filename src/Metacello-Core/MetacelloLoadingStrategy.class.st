Class {
	#name : 'MetacelloLoadingStrategy',
	#superclass : 'Object',
	#instVars : [
		'gofer',
		'loader'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'switch-strategy' }
MetacelloLoadingStrategy >> changeToAtomicWithLoader: anObject [

	^ MetacelloAtomicLoadingStrategy new
		  gofer: gofer;
		  loader: self loader;
		  yourself
]

{ #category : 'switch-strategy' }
MetacelloLoadingStrategy >> changeToLinearWithLoader: aLoader [

	self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloLoadingStrategy >> gofer [

	^ gofer
]

{ #category : 'accessing' }
MetacelloLoadingStrategy >> gofer: anObject [

	gofer := anObject
]

{ #category : 'accessing' }
MetacelloLoadingStrategy >> loader [
	
	^ loader
]

{ #category : 'accessing' }
MetacelloLoadingStrategy >> loader: aLoader [

	loader := aLoader
]
