"
I am a morph that prints the keys in the keyboard events.
Open the utility using:

    KeyPrinterMorph open

then click into it to activate the window and press keys on
your keyboard to display the key code.
"
Class {
	#name : #KeyPrinterMorph,
	#superclass : #Morph,
	#category : #'Tools-Debugger'
}

{ #category : #'open/close' }
KeyPrinterMorph class >> open [
	<script>
	
	self new openInWorld
]

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

	self
		extent: 400@200; 
		openInWindowLabeled: 'KeyPrinter (click/activate and press a key)'
]
