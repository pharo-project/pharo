Class {
	#name : #SizeReporter,
	#superclass : #AbstractReporter,
	#category : #'Tool-SystemReporter'
}

{ #category : #'instance creation' }
SizeReporter class >> open [
	<script>
	^ super open
]

{ #category : #'system menu' }
SizeReporter class >> systemReporterMenuOn: aBuilder [ 
	<worldMenu>
	
	(aBuilder item: #'Size Reporter')
		parent: #System;
		action: [ self open ];
		help: 'Reports different numbers on the size of the image.'
]

{ #category : #initialization }
SizeReporter >> initialize [
	super initialize.
	self
		add: #'# classes' method: #reportNbClasses:;
		add: #'# traits' method: #reportNbTraits:;
		add: #'Total LOC' method: #reportLOC:.
	categoriesSelected := Set with: #'# classes'.
	self updateReport
]

{ #category : #accessing }
SizeReporter >> label [
	^ 'Size Reporter' 
]

{ #category : #'instance creation' }
SizeReporter >> open [
	<script: 'self new open'>
	super open
]

{ #category : #reporting }
SizeReporter >> reportLOC: aStream [
	self header: 'Number of LinesOfCode' on: aStream.
	aStream 
		nextPutAll: 'a lot !' ; cr

]

{ #category : #reporting }
SizeReporter >> reportNbClasses: aStream [
	| imageClasses |
	imageClasses := Smalltalk globals allClasses.
	self header: 'Number of classes' on: aStream.
	aStream 
		nextPutAll: imageClasses size asString; cr ;
		nextPutAll: 'Tests: ' ;
		nextPutAll: (imageClasses count: #isTestCase) asString ; cr ;
		"very long to compute :-(
		nextPutAll: 'Without instances: ' ;
		nextPutAll: (imageClasses count: [:c | c allInstances isEmpty]) asString ; cr ;"
		nextPutAll: 'Manifests: ' ;
		nextPutAll: (imageClasses count: #isManifest) asString; cr ;
		nextPutAll: 'BaselineOf-s: ' ;
		nextPutAll: (imageClasses count: [:c | c name beginsWith: 'BaselineOf' ]) asString ; cr
]

{ #category : #reporting }
SizeReporter >> reportNbTraits: aStream [
	| imageTraits |
	imageTraits := Smalltalk allTraits.
	self header: 'Number of traits' on: aStream.
	aStream 
		nextPutAll: imageTraits size asString; cr ;
		nextPutAll: 'not used: ' ;
		nextPutAll: (imageTraits count: [ :t | t users isEmpty ]) asString; cr

]
