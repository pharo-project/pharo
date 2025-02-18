"
Example for a package class node
"
Class {
	#name : 'PackageClassNodeExample',
	#superclass : 'PackageAbstractNodeExample',
	#category : 'Morphic-Examples-MorphTreeWidget',
	#package : 'Morphic-Examples',
	#tag : 'MorphTreeWidget'
}

{ #category : 'accessing' }
PackageClassNodeExample >> browseItem [
	(Smalltalk  toolNamed: #browser) openOnClass: self item
]

{ #category : 'menu' }
PackageClassNodeExample >> childNodeClassFromItem: anItem [
	^PackageMethodCategoryNodeExample
]

{ #category : 'menu' }
PackageClassNodeExample >> childrenItems [

	^ self item protocolNames
]

{ #category : 'menu' }
PackageClassNodeExample >> methodsInCategory: aProtocol [

	^ self item selectorsInProtocol: aProtocol
]
