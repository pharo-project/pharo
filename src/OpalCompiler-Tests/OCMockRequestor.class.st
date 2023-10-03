"
I'm a test utility class that tries to mock a requestor API and just log callbacks
"
Class {
	#name : 'OCMockRequestor',
	#superclass : 'Object',
	#instVars : [
		'notifyList',
		'interactive',
		'isScripting',
		'text'
	],
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'binding' }
OCMockRequestor >> bindingOf: aSymbol [

	^ nil
]

{ #category : 'interactive error protocol' }
OCMockRequestor >> correctFrom: anInteger to: anInteger2 with: aString [

	text := (text first: anInteger - 1) , aString , (text copyFrom: anInteger2 + 1 to: text size)
]

{ #category : 'binding' }
OCMockRequestor >> hasBindingOf: aSymbol [

	^ false
]

{ #category : 'initialization' }
OCMockRequestor >> initialize [

	notifyList := OrderedCollection new.
	interactive := false
]

{ #category : 'accessing' }
OCMockRequestor >> interactive [
	^ interactive
]

{ #category : 'accessing' }
OCMockRequestor >> interactive: anObject [

	interactive := anObject
]

{ #category : 'accessing' }
OCMockRequestor >> isScripting [
	^ isScripting
]

{ #category : 'testing' }
OCMockRequestor >> isScripting: aBoolean [

	isScripting := aBoolean
]

{ #category : 'interactive error protocol' }
OCMockRequestor >> notify: aString at: anInteger in: aString3 [

	notifyList add: { aString. anInteger. aString3 }
]

{ #category : 'accessing' }
OCMockRequestor >> notifyList [

	^ notifyList
]

{ #category : 'accessing' }
OCMockRequestor >> text [

	^ text
]

{ #category : 'accessing' }
OCMockRequestor >> text: aString [

	text := aString
]

{ #category : 'accessing' }
OCMockRequestor >> textMorph [

	self error: 'Seriously, a Morph?'
]
