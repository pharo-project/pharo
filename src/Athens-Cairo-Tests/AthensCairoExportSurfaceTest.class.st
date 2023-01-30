Class {
	#name : #AthensCairoExportSurfaceTest,
	#superclass : #TestCase,
	#instVars : [
		'surface'
	],
	#category : #'Athens-Cairo-Tests-Core'
}

{ #category : #testing }
AthensCairoExportSurfaceTest class >> isAbstract [

	^ self = AthensCairoExportSurfaceTest
]

{ #category : #accessing }
AthensCairoExportSurfaceTest >> athensSurfaceClass [

	^ self subclassResponsibility
]

{ #category : #accessing }
AthensCairoExportSurfaceTest >> fileName [

	^ self className asString
]

{ #category : #accessing }
AthensCairoExportSurfaceTest >> fileReference [

	^ self fileName asFileReference
]

{ #category : #running }
AthensCairoExportSurfaceTest >> setUp [

	super setUp.
	self fileReference ensureDelete
]

{ #category : #running }
AthensCairoExportSurfaceTest >> tearDown [

	surface close.
	self fileReference ensureDelete.
	super tearDown
]

{ #category : #tests }
AthensCairoExportSurfaceTest >> testExportBoxes [

	| path |
	surface := self athensSurfaceClass
		extent: 100@100
		fileName: self fileName.
	path := surface createRectanglePath: (0@0 corner: 20@20).

	surface drawDuring: [:canvas |
		surface clear.
		canvas pathTransform loadIdentity.

		canvas setPaint: Color red.
		canvas pathTransform translateX: 5 Y: 5.
		canvas drawShape: path.

		canvas setPaint: Color blue.
		canvas pathTransform translateX: 5 Y: 5.
		canvas drawShape: path ].

	"Make cairo write and close the file."
	surface close.

	self assert: self fileReference exists.
	self assert: self fileReference size > 400
]

{ #category : #tests }
AthensCairoExportSurfaceTest >> testExtent [

	surface := self athensSurfaceClass
		extent: 100@50
		fileName: self fileName.
	self assert: surface extent equals: 100@50
]

{ #category : #tests }
AthensCairoExportSurfaceTest >> testHeight [

	surface := self athensSurfaceClass
		extent: 100@50
		fileName: self fileName.
	self assert: surface height equals: 50
]

{ #category : #tests }
AthensCairoExportSurfaceTest >> testWidth [

	surface := self athensSurfaceClass
		extent: 100@50
		fileName: self fileName.
	self assert: surface width equals: 100
]
