"
An OSVMWindowHandle is created by VMWindowDriver.
 
"
Class {
	#name : #OSVMWindowHandle,
	#superclass : #OSWindowHandle,
	#instVars : [
		'fullscreen'
	],
	#category : #'OSWindow-VM'
}

{ #category : #accessing }
OSVMWindowHandle >> bounds [
	^ self position corner: self extent
]

{ #category : #accessing }
OSVMWindowHandle >> bounds: bounds [
	self position: bounds origin.
	self extent: bounds extent
]

{ #category : #accessing }
OSVMWindowHandle >> clipboardText [
	<primitive: 141>
	^ ''
]

{ #category : #accessing }
OSVMWindowHandle >> clipboardText: aString [
	<primitive: 141>
]

{ #category : #accessing }
OSVMWindowHandle >> depth: depthInteger width: widthInteger height: heightInteger fullscreen: aBoolean [
	"Force Pharo's window (if there's one) into a new size and depth."
	"DisplayScreen depth: 8 width: 1024 height: 768 fullscreen: false"

	<primitive: 92>
	self primitiveFailed 
]

{ #category : #accessing }
OSVMWindowHandle >> extent [
	<primitive: 106>
	^ 640@480
]

{ #category : #accessing }
OSVMWindowHandle >> extent: extent [
	self depth: self screenDepth width: extent x height: extent y fullscreen: fullscreen
]

{ #category : #accessing }
OSVMWindowHandle >> fullscreen: aBoolean [
]

{ #category : #accessing }
OSVMWindowHandle >> hide [
	"Do nothing for now"
]

{ #category : #initialize }
OSVMWindowHandle >> initWithAttributes: attributes [
	self extent: attributes extent
]

{ #category : #initialization }
OSVMWindowHandle >> initialize [
	super initialize.
	fullscreen := false
]

{ #category : #testing }
OSVMWindowHandle >> isValid [
	^ true
]

{ #category : #'instance creation' }
OSVMWindowHandle >> newFormRenderer: form [ 
	^ renderer := OSVMFormRenderer new 
		form: form;
		window: self;
		yourself
]

{ #category : #accessing }
OSVMWindowHandle >> position [
	"Ignored"
]

{ #category : #accessing }
OSVMWindowHandle >> position: newPosition [
	"Ignored"
]

{ #category : #accessing }
OSVMWindowHandle >> screenDepth [
	<primitive: #primitiveScreenDepth >
	^ self renderer form depth
]

{ #category : #accessing }
OSVMWindowHandle >> setMouseCursor: cursorWithMask [

	Cursor currentCursor == cursorWithMask ifFalse: [  
		Cursor currentCursor: cursorWithMask ].
]

{ #category : #accessing }
OSVMWindowHandle >> show [
	"Do nothing for now"
]

{ #category : #accessing }
OSVMWindowHandle >> title [
	^ ''
]

{ #category : #accessing }
OSVMWindowHandle >> title: newTitle [
	"Ignored"
]
