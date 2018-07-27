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

{ #category : #'initialize-release' }
RubSmalltalkScriptingMode >> isScripting [
	^ true
]

{ #category : #parsing }
RubSmalltalkScriptingMode >> parseSource: aString [

	^RBParser parseFaultyExpression: aString
]

{ #category : #'initialize-release' }
RubSmalltalkScriptingMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged.
	self textArea shoutStyler beForSmalltalkScripting
]
