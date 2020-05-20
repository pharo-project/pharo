"
I'm an example that shows a tiger that moves via rotation, translation and zoom in a Morph window.
"
Class {
	#name : #VGTigerDemo,
	#superclass : #StandardWindow,
	#instVars : [
		'surface',
		'paths',
		'rotationSpeed',
		'rotation',
		'extent',
		'scale',
		'translation',
		'time',
		'frames',
		'zoom',
		'example'
	],
	#category : #'Athens-Examples-Utilities'
}

{ #category : #drawing }
VGTigerDemo >> drawOn: aCanvas [

	super drawOn: aCanvas.
	aCanvas drawImage: surface asForm at: self layoutBounds origin.


]

{ #category : #initialization }
VGTigerDemo >> initialize [ 

	super initialize.
	surface := AthensCairoSurface extent: self surfaceExtent.
	example := AthensTiger on: surface.
	rotationSpeed := 1.
	rotation := 0.
	extent := surface extent.
	scale := extent x / AthensTiger tigerMaxY.
	frames := 0.


]

{ #category : #running }
VGTigerDemo >> openAnimated [
	<script: 'self new runDemo'>

	self
		open;
		extent: self surfaceExtent + (50@50);
		title: 'Athens Tiger Demo';
		startStepping.
]

{ #category : #'stepping and presenter' }
VGTigerDemo >> step [ 

	rotation := rotation + rotationSpeed.
	frames := frames + 1.
	zoom := (frames / 100) sin abs.
	
	example
		rotation: rotation;
		zoom: zoom;
		scale: scale;
		draw.

	self changed
]

{ #category : #stepping }
VGTigerDemo >> stepTime [
	^ 50
]

{ #category : #initialization }
VGTigerDemo >> surfaceExtent [
	^ 400 @ 400
]
