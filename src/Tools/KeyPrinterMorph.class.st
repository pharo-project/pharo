"
I am a morph that prints the keys in the keyboard events.

KeyPrinterMorph new openInWorld
"
Class {
	#name : #KeyPrinterMorph,
	#superclass : #Morph,
	#category : 'Tools-Debugger'
}

{ #category : #'event handling' }
KeyPrinterMorph >> handlesKeyboard: evt [
	^ true
]

{ #category : #'event handling' }
KeyPrinterMorph >> handlesMouseDown: evt [
	^ true
]

{ #category : #'event handling' }
KeyPrinterMorph >> keyDown: anEvent [
	UIManager default inform: anEvent key asString.
]

{ #category : #'event handling' }
KeyPrinterMorph >> mouseDown: event [ 
	super mouseDown: event.
	World activeHand newKeyboardFocus: self
]

{ #category : #initialize }
KeyPrinterMorph >> openInWorld [
	self  openInWindowLabeled: 'KeyPrinter'
]
