Class {
	#name : 'MetacelloLoadingStrategy',
	#superclass : 'Object',
	#instVars : [
		'gofer'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'switch-strategy' }
MetacelloLoadingStrategy >> changeToAtomic [

	^ MetacelloAtomicLoadingStrategy new
		  gofer: gofer;
		  yourself
]

{ #category : 'switch-strategy' }
MetacelloLoadingStrategy >> changeToLinear [

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
