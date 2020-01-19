Class {
	#name : #SpCodePrintItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodePrintItCommand class >> defaultIconName [
	^#smallPrintIt
]

{ #category : #default }
SpCodePrintItCommand class >> defaultName [

	^ 'Print it'
]

{ #category : #default }
SpCodePrintItCommand class >> defaultShortcutKey [

	^ $p command mac
		| $p control win
		| $p control unix
]

{ #category : #execution }
SpCodePrintItCommand >> execute [
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	
	self evaluateSelectionAndDo: [ :result | 
		self printObject: result ]
]

{ #category : #private }
SpCodePrintItCommand >> printObject: anObject [
	| printString |
	
	printString := [ anObject printString ]
		on: Error
		do: [ '<error in printString: try ''Inspect it'' to debug>' ].
	self afterSelectionInsertAndSelect: (self toPrintableString: printString)
]

{ #category : #private }
SpCodePrintItCommand >> toPrintableString: printString [

	self flag: #TODO. "This is maybe not the best? 
	I think I need to add a popover, but that then means I need to 
	have a special way to handle this special 'insertion' (to transmit the correct insertion 
	to the presenter's adapter)."
	^ String streamContents: [ :stream |
		stream 
			<< ' "'
			<< printString 
			<< '"' ]
]
