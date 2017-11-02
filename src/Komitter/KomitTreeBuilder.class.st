"
I am builder class used to generate the tree nodes for a package or for all the dirty packages
"
Class {
	#name : #KomitTreeBuilder,
	#superclass : #Object,
	#category : 'Komitter-UI'
}

{ #category : #protocol }
KomitTreeBuilder class >> buildRootForPackage: aKOPackage [

	^ self new
		buildRootForPackage: aKOPackage
]

{ #category : #protocol }
KomitTreeBuilder class >> rootsForStagingArea: aStagingArea [

	^ aStagingArea packages 
		collect: [ :each | self buildRootForPackage: each ]
		thenSelect: [:each | each isEmpty not ]
]

{ #category : #protocol }
KomitTreeBuilder >> buildRootForPackage: aPackage [

	^ KomitPackageNode new
		content: aPackage;
		yourself
]
