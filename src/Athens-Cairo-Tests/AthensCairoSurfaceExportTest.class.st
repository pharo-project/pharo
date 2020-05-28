Class {
	#name : #AthensCairoSurfaceExportTest,
	#superclass : #TestCase,
	#category : #'Athens-Cairo-Tests-Core'
}

{ #category : #testing }
AthensCairoSurfaceExportTest class >> isAbstract [
	^ self = AthensCairoSurfaceExportTest
]

{ #category : #tests }
AthensCairoSurfaceExportTest >> athensSurfaceClass [
	^ self subclassResponsibility
]

{ #category : #running }
AthensCairoSurfaceExportTest >> fileName [
	^ self className asString
]

{ #category : #running }
AthensCairoSurfaceExportTest >> fileReference [
	^ self fileName asFileReference
]

{ #category : #running }
AthensCairoSurfaceExportTest >> setUp [
	super setUp.
	self fileReference ensureDelete
]

{ #category : #running }
AthensCairoSurfaceExportTest >> tearDown [
	self fileReference ensureDelete.
	super tearDown.
]

{ #category : #tests }
AthensCairoSurfaceExportTest >> testExportBoxes [
	| path surface |
	surface := self athensSurfaceClass
		extent: 100@100
		fileName: self fileName.
	path :=  surface createRectanglePath: (0@0 corner: 20@20).

	surface drawDuring: [:canvas |
		surface clear.
		canvas pathTransform loadIdentity.
		canvas setPaint: Color red.
		canvas pathTransform translateX: 5 Y: 5.
		canvas drawShape: path.
		canvas setPaint: Color blue.
		canvas pathTransform translateX: 5 Y: 5.
		canvas drawShape: path ].
	
	"-- Required steps at the end of the export --"

	surface showPage. "The page is finished (for PDF)"
	surface finish. "The file is finished"

	"Call destroy functions (the garbage collector will do it via finalize)"
	surface := nil. 
	Smalltalk garbageCollect.

	self assert: self fileReference exists.
	self assert: self fileReference size > 400.
]

{ #category : #tests }
AthensCairoSurfaceExportTest >> testExtent [
	| surface |
	surface := self athensSurfaceClass
		extent: 100@50
		fileName: self fileName.
	self assert: surface extent equals: 100@50
]

{ #category : #tests }
AthensCairoSurfaceExportTest >> testHeight [
	| surface |
	surface := self athensSurfaceClass
		extent: 100@50
		fileName: self fileName.
	self assert: surface height equals: 50
]

{ #category : #tests }
AthensCairoSurfaceExportTest >> testWidth [
	| surface |
	surface := self athensSurfaceClass
		extent: 100@50
		fileName: self fileName.
	self assert: surface width equals: 100
]
