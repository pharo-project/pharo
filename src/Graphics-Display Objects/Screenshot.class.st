"
I implement the logic to do screenshots.
I can do a full screenshot or an area of the screen selected by the user.
I handle the user interaction to get the area to make the screenshot.

The method #makeAScreenshot handles the menu option to make screen shots.
"
Class {
	#name : 'Screenshot',
	#superclass : 'Object',
	#category : 'Graphics-Display Objects-Utilities',
	#package : 'Graphics-Display Objects',
	#tag : 'Utilities'
}

{ #category : 'world menu items' }
Screenshot class >> screenShotCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Screenshot)
		parent: #System;
		target: self new;
		order: 99;
		selector: #makeAScreenshot;
		label: 'Screenshot';
		help: 'Take a screenshot';
		iconName: #smallScreenshot
]

{ #category : 'operations' }
Screenshot >> formScreenshotFromUserSelection [
	
	^ self formScreenshotFromUserSelection: 1 @ 1
]

{ #category : 'operations' }
Screenshot >> formScreenshotFromUserSelection: initialPoint [
	
	| rect |
	
	rect := self askRectangleFromUserWithGrid: initialPoint.
	^ Form fromDisplay: (rect origin extent: (rect extent max: initialPoint))
]
