"
I am a simple interface to the Display screen canvas to draw text.
"
Class {
	#name : #EDDisplayInterface,
	#superclass : #Object,
	#instVars : [
		'window',
		'renderer',
		'characterReceivedAction',
		'windowClosedAction',
		'extent'
	],
	#category : #'EmergencyDebugger-View'
}

{ #category : #'accessing - properties' }
EDDisplayInterface >> black [
	^ Color black
]

{ #category : #accessing }
EDDisplayInterface >> clear [
	
	self drawDuring: [ :canvas | 
		canvas
			frameAndFillRectangle: self frame
			fillColor: self white
			borderWidth: 2
			borderColor: self red ]
]

{ #category : #'private - display' }
EDDisplayInterface >> closeWindow [

	windowClosedAction 
		ifNotNil: [ windowClosedAction value ]
		ifNil: [ self terminateWindow ]
]

{ #category : #'private - text' }
EDDisplayInterface >> composeParagraphWith: aText inFrame: aRect [
	| paragraph |

	paragraph := Paragraph new.
	paragraph
		compose: aText
		style: self textStyle
		from: 1
		in: ((aRect insetBy: 4) withHeight: aRect height).
	
	^ paragraph
]

{ #category : #'private - display' }
EDDisplayInterface >> display: aParagraph in: aRect [

	self drawDuring: [ :canvas |
		canvas
			paragraph: aParagraph
			bounds: aRect
			color: self black ]
]

{ #category : #'private - display' }
EDDisplayInterface >> drawDuring: aBlock [

	self ensureWindowOpen.
	renderer deferUpdatesWhile: [
		renderer drawDuring: aBlock.
		renderer present ]
]

{ #category : #'private - display' }
EDDisplayInterface >> ensureWindowOpen [
	| attrs |
	
	window ifNotNil: [ 
		window isVisible ifFalse: [ window show ].
		^ self ].
	
	attrs := OSWindowAttributes new.
	attrs 
		title: 'Emergency Debugger';
		extent: self extent;
		resizable: false.
		
	window := OSWindow 
		createWithAttributes: attrs
		eventHandler: (EDDisplayEventHandler on: self).
	renderer := window newFormRenderer: (Form extent: self extent depth: 32).
]

{ #category : #'private - display' }
EDDisplayInterface >> extent [

	^ extent ifNil: [ 
		extent := DisplayScreen actualScreenSize min: (650@700) ]
]

{ #category : #'private - display' }
EDDisplayInterface >> frame [

	^ 0@0 corner: self extent
]

{ #category : #'accessing - properties' }
EDDisplayInterface >> gray [
	^ Color gray
]

{ #category : #'private - text' }
EDDisplayInterface >> pushChar: char [

	characterReceivedAction ifNil: [ ^ self ].
	characterReceivedAction value: char
]

{ #category : #'accessing - properties' }
EDDisplayInterface >> red [
	^ Color red
]

{ #category : #accessing }
EDDisplayInterface >> show: aText [
	
	self
		display: (self 
			composeParagraphWith: aText 
			inFrame: self frame)
		in: self frame
]

{ #category : #accessing }
EDDisplayInterface >> terminateWindow [

	window ifNil: [ ^ self ].
	window destroy
]

{ #category : #'private - text' }
EDDisplayInterface >> textStyle [
	^ TextStyle default
]

{ #category : #events }
EDDisplayInterface >> whenCharacterReceivedDo: aBlock [

	characterReceivedAction := aBlock
]

{ #category : #events }
EDDisplayInterface >> whenWindowClosedDo: aBlock [
	"informs when window was closed from buttons"

	windowClosedAction := aBlock
]

{ #category : #'accessing - properties' }
EDDisplayInterface >> white [
	^ Color white
]
