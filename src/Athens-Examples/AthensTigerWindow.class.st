"
I'm an example that shows a tiger that moves via rotation, translation and zoom in a Morph window.
"
Class {
	#name : #AthensTigerWindow,
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
	#category : #'Athens-Examples-Morphic'
}

{ #category : #running }
AthensTigerWindow class >> openAnimated [
	<script>

	^ self new
		open;
		extent: self surfaceExtent + (50@50);
		title: 'Athens Tiger Demo';
		startStepping
]

{ #category : #accessing }
AthensTigerWindow class >> surfaceExtent [
	^ 400 @ 400
]

{ #category : #drawing }
AthensTigerWindow >> drawOn: aCanvas [

	super drawOn: aCanvas.
	aCanvas drawImage: surface asForm at: self layoutBounds origin.


]

{ #category : #initialization }
AthensTigerWindow >> initialize [ 

	super initialize.
	surface := AthensCairoSurface extent: self class surfaceExtent.
	example := AthensTiger on: surface.
	rotationSpeed := 1.
	rotation := 0.
	extent := surface extent.
	scale := extent x / AthensTiger tigerMaxY.
	frames := 0.


]

{ #category : #'stepping and presenter' }
AthensTigerWindow >> step [ 

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
AthensTigerWindow >> stepTime [
	^ 50
]
