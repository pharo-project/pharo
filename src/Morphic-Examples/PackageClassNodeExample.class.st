"
Example for a package class node
"
Class {
	#name : #PackageClassNodeExample,
	#superclass : #PackageAbstractNodeExample,
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #accessing }
PackageClassNodeExample >> browseItem [
	Smalltalk tools browser openOnClass: self item
]

{ #category : #menu }
PackageClassNodeExample >> childNodeClassFromItem: anItem [
	^PackageMethodCategoryNodeExample
]

{ #category : #menu }
PackageClassNodeExample >> childrenItems [
	^ self item organization categories 

]

{ #category : #menu }
PackageClassNodeExample >> methodsInCategory: aCat [
	^ self item  selectorsInProtocol: aCat
]
