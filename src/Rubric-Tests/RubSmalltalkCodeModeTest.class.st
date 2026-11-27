"
I test `RubSmalltalkCodeMode`, the editing mode of Rubric's Smalltalk code editor.
"
Class {
	#name : 'RubSmalltalkCodeModeTest',
	#superclass : 'TestCase',
	#category : 'Rubric-Tests',
	#package : 'Rubric-Tests'
}

{ #category : 'tests' }
RubSmalltalkCodeModeTest >> testFormatterDefaultsToTheEditedClassFormatter [

	| mode |
	mode := RubSmalltalkCodeMode new.
	mode classOrMetaClass: self class.

	self assert: mode formatter class equals: self class formatter class
]

{ #category : 'tests' }
RubSmalltalkCodeModeTest >> testFormatterSetOnTheModeIsUsed [

	| mode formatter |
	formatter := RubHardcodedTestFormatter new.
	mode := RubSmalltalkCodeMode new.
	mode classOrMetaClass: self class.
	mode formatter: formatter.

	self assert: mode formatter identicalTo: formatter
]
