"
I represent the mode to use for editing smalltalk script (class creation script for example)
"
Class {
	#name : #RubSmalltalkScriptingMode,
	#superclass : #RubSmalltalkCodeMode,
	#category : #'Rubric-Editing-Code'
}

{ #category : #menu }
RubSmalltalkScriptingMode class >> editingModeMenuOn: aBuilder [
	"Specify the menu used to select an editing mode"

	<contextMenu>
	<RubEditingModeMenu>
	(aBuilder item: self label translated)
		action: [:model | model textArea editingMode: self new]
]

{ #category : #accessing }
RubSmalltalkScriptingMode class >> label [
	^ 'Smalltalk script'
]

{ #category : #testing }
RubSmalltalkScriptingMode >> isScripting [
	^ true
]

{ #category : #parsing }
RubSmalltalkScriptingMode >> parseSource: aString [

	^RBParser parseFaultyExpression: aString
]

{ #category : #shout }
RubSmalltalkScriptingMode >> updateStyler [
	super updateStyler.
	self textArea shoutStyler beForSmalltalkScripting
]

{ #category : #update }
RubSmalltalkScriptingMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged.
	self textArea shoutStyler beForSmalltalkScripting
]
