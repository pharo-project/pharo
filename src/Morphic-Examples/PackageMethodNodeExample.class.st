"
Example for a package method node
"
Class {
	#name : 'PackageMethodNodeExample',
	#superclass : 'PackageAbstractNodeExample',
	#category : 'Morphic-Examples-MorphTreeWidget',
	#package : 'Morphic-Examples',
	#tag : 'MorphTreeWidget'
}

{ #category : 'accessing' }
PackageMethodNodeExample >> browseItem [
	(Smalltalk tools toolNamed: #browser) 
		openOnClass: self itemClass  
		selector: self selector
]

{ #category : 'results' }
PackageMethodNodeExample >> compiledMethod [
	^ self itemClass
		compiledMethodAt: self selector
		ifAbsent: [self itemClass class
				compiledMethodAt: self selector
				ifAbsent: []]
]

{ #category : 'private' }
PackageMethodNodeExample >> itemClass [
	^ self parentNode parentNode item
]

{ #category : 'accessing' }
PackageMethodNodeExample >> selector [
	^ self item
]
