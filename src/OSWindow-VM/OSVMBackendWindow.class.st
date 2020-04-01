"
An OSVMWindowHandle is created by VMWindowDriver.
 
"
Class {
	#name : #OSVMBackendWindow,
	#superclass : #OSBackendWindow,
	#instVars : [
		'fullscreen'
	],
	#category : #'OSWindow-VM'
}

{ #category : #accessing }
OSVMBackendWindow >> bounds [
	^ self position corner: self extent
]

{ #category : #accessing }
OSVMBackendWindow >> bounds: bounds [
	self position: bounds origin.
	self extent: bounds extent
]

{ #category : #accessing }
OSVMBackendWindow >> clipboardText [
	<primitive: 141>
	^ ''
]

{ #category : #accessing }
OSVMBackendWindow >> clipboardText: aString [
	<primitive: 141>
]

{ #category : #accessing }
OSVMBackendWindow >> depth: depthInteger width: widthInteger height: heightInteger fullscreen: aBoolean [
	"Force Pharo's window (if there's one) into a new size and depth."
	"DisplayScreen depth: 8 width: 1024 height: 768 fullscreen: false"

	<primitive: 92>
	self primitiveFailed 
]

{ #category : #accessing }
OSVMBackendWindow >> extent [
	<primitive: 106>
	^ 640@480
]

{ #category : #accessing }
OSVMBackendWindow >> extent: extent [
	self depth: self screenDepth width: extent x height: extent y fullscreen: fullscreen
]

{ #category : #accessing }
OSVMBackendWindow >> fullscreen: aBoolean [
]

{ #category : #accessing }
OSVMBackendWindow >> hide [
	"Do nothing for now"
]

{ #category : #initialize }
OSVMBackendWindow >> initWithAttributes: attributes [
	self extent: attributes extent
]

{ #category : #initialization }
OSVMBackendWindow >> initialize [
	super initialize.
	fullscreen := false
]

{ #category : #testing }
OSVMBackendWindow >> isValid [
	^ true
]

{ #category : #'instance creation' }
OSVMBackendWindow >> newFormRenderer: form [ 
	^ renderer := OSVMFormRenderer new 
		form: form;
		backendWindow: self;
		yourself
]

{ #category : #accessing }
OSVMBackendWindow >> position [
	"Ignored"
]

{ #category : #accessing }
OSVMBackendWindow >> position: newPosition [
	"Ignored"
]

{ #category : #accessing }
OSVMBackendWindow >> screenDepth [
	<primitive: #primitiveScreenDepth >
	^ self renderer form depth
]

{ #category : #accessing }
OSVMBackendWindow >> setMouseCursor: cursorWithMask [

	self currentWorld currentCursor == cursorWithMask ifFalse: [  
		self currentWorld currentCursor: cursorWithMask ].
]

{ #category : #accessing }
OSVMBackendWindow >> show [
	"Do nothing for now"
]

{ #category : #accessing }
OSVMBackendWindow >> title [
	^ ''
]

{ #category : #accessing }
OSVMBackendWindow >> title: newTitle [
	"Ignored"
]
