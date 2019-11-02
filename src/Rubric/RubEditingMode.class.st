"
I am an abstract class to choose the editing mode of the text area 
"
Class {
	#name : #RubEditingMode,
	#superclass : #Object,
	#instVars : [
		'textArea',
		'previousEditorClass',
		'acceptAllowed'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #keymap }
RubEditingMode class >> buildTabKeymapOn: aBuilder [
	<keymap>
	
]

{ #category : #menu }
RubEditingMode class >> extendedSearchMenuOn: aBuilder [
	(aBuilder item: #'extendedSearch' translated)
		label: 'Code search...' translated;
		withSeparatorAfter;
		with: [ 
					(aBuilder item: #'Browse full class' translated)
						keyText: 'b';
						selector: #browseFullClass.
					(aBuilder item: #'Senders of it' translated)
						keyText: 'n';
						selector: #sendersOfIt.
					(aBuilder item: #'Implementors of it' translated)
						keyText: 'm';
						selector: #implementorsOfIt.
					(aBuilder item: #'References to it' translated)
						keyText: 'N';
						selector: #referencesToIt;
						withSeparatorAfter.
					(aBuilder item: #'Selectors containing it' translated)
						keyText: 'W';
						selector: #methodNamesContainingIt.
					(aBuilder item: #'Case insensitive method literal strings with it' translated)
						keyText: 'E';
						selector: #methodStringsContainingit.
					(aBuilder item: #'Case sensitive method literal strings with it' translated)
						keyText: 'E';
						selector: #methodCaseSensitiveStringsContainingit.
					(aBuilder item: #'Method source with it' translated)
						selector: #methodSourceContainingIt;
						withSeparatorAfter.
					(aBuilder item: #'Class names containing it' translated) selector: #classNamesContainingIt.
					(aBuilder item: #'Class comments with it' translated) selector: #classCommentsContainingIt ]
]

{ #category : #menu }
RubEditingMode class >> menuKeyword [
	"Returns the name of the pragma to use to build the menu"
	^ self subclassResponsibility 
]

{ #category : #accessing }
RubEditingMode >> acceptAllowed [
	^ acceptAllowed
]

{ #category : #accessing }
RubEditingMode >> acceptAllowed: anObject [
	acceptAllowed := anObject
]

{ #category : #accessing }
RubEditingMode >> editor [
	^self textArea editor
]

{ #category : #accessing }
RubEditingMode >> editorClass [
	^ RubTextEditor 
]

{ #category : #initialization }
RubEditingMode >> initialize [
	super initialize.
	acceptAllowed := true
]

{ #category : #accessing }
RubEditingMode >> interactive [

	"when returning true, it avoids that we can assign to class, i.e., Array := 55."
	
	^ true
]

{ #category : #testing }
RubEditingMode >> isCompletionEnabled [
	^ false
]

{ #category : #accessing }
RubEditingMode >> label [
	^ self class label
]

{ #category : #menu }
RubEditingMode >> menu [ 
		"Answer the menu to be presented "

	^ (PragmaMenuBuilder pragmaKeyword: self menuKeyword  model: self textArea ) menu
]

{ #category : #menu }
RubEditingMode >> menuKeyword [
	^ self class menuKeyword
]

{ #category : #accessing }
RubEditingMode >> model [
	^ self textArea model
]

{ #category : #initialization }
RubEditingMode >> plugin: aTextArea [ 
	textArea := aTextArea.
	previousEditorClass := self textArea editorClass.
	previousEditorClass = self editorClass
		ifFalse: [ 
			self textArea editorClass: self editorClass.
			self textArea releaseEditor].
	self updateTextAreaWhenPlugged
]

{ #category : #menu }
RubEditingMode >> shiftMenu [ 
		"Answer the menu to be presented with the shift key pressed"

	^ self menu
]

{ #category : #accessing }
RubEditingMode >> textArea [
	^ textArea
]

{ #category : #accessing }
RubEditingMode >> textArea: aTextArea [
	textArea := aTextArea
]

{ #category : #accessing }
RubEditingMode >> textStyle [
	^ self textArea textStyle
]

{ #category : #initialization }
RubEditingMode >> unplug [
	previousEditorClass = self textArea editor class
		ifFalse: [ 
			self textArea editorClass: previousEditorClass.
			self textArea releaseEditor ].
	self updateTextAreaWhenUnplugged.
	textArea := nil.

]

{ #category : #initialization }
RubEditingMode >> updateTextAreaWhenPlugged [
	self textArea
		restoreSelectionIndexesAfter: [ 
			| t |
			t := self textArea text copy.
			self textArea updateTextWith: ''.
			self textArea updateTextWith: t ]
]

{ #category : #initialization }
RubEditingMode >> updateTextAreaWhenUnplugged [

]
