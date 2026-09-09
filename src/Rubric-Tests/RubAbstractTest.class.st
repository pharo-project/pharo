Class {
	#name : 'RubAbstractTest',
	#superclass : 'TestCase',
	#instVars : [
		'string',
		'editor'
	],
	#category : 'Rubric-Tests-Editing-Core',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Core'
}

{ #category : 'running' }
RubAbstractTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubEditingArea new.
	"Add text with a paragraph"
	string := 'Lorem ipsum '
]
