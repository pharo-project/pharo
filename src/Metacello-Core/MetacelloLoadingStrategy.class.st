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

{ #category : 'as yet unclassified' }
MetacelloLoadingStrategy >> changeToAtomicWithLoader: anObject [

	^ MetacelloAtomicLoadingStrategy new
		  gofer: gofer;
		  loader: self loader;
		  yourself
]

{ #category : 'as yet unclassified' }
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

{ #category : 'as yet unclassified' }
MetacelloLoadingStrategy >> linear [

	^ MetacelloLinearLoadingStrategy new
		  gofer: gofer;
		  loader: self loader;
		  yourself
]

{ #category : 'accessing' }
MetacelloLoadingStrategy >> loader [
	
	^ loader
]

{ #category : 'accessing' }
MetacelloLoadingStrategy >> loader: aLoader [

	loader := aLoader
]
