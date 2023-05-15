"
I'm a test utility class that tries to mock a requestor API and just log callbacks
"
Class {
	#name : #OCMockRequestor,
	#superclass : #Object,
	#instVars : [
		'notifyList',
		'interactive',
		'isScripting'
	],
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #binding }
OCMockRequestor >> bindingOf: aSymbol [

	^ nil
]

{ #category : #'interactive error protocol' }
OCMockRequestor >> correctFrom: anInteger to: anInteger2 with: aString [

	self error: 'I do not want to mock this one'
]

{ #category : #binding }
OCMockRequestor >> hasBindingOf: aSymbol [

	^ false
]

{ #category : #initialization }
OCMockRequestor >> initialize [

	notifyList := OrderedCollection new.
	interactive := false
]

{ #category : #accessing }
OCMockRequestor >> interactive [
	^ interactive
]

{ #category : #accessing }
OCMockRequestor >> interactive: anObject [

	interactive := anObject
]

{ #category : #accessing }
OCMockRequestor >> isScripting [
	^ isScripting
]

{ #category : #testing }
OCMockRequestor >> isScripting: aBoolean [

	isScripting := aBoolean
]

{ #category : #'interactive error protocol' }
OCMockRequestor >> notify: aString at: anInteger in: aString3 [

	notifyList add: { aString. anInteger. aString3 }
]

{ #category : #accessing }
OCMockRequestor >> notifyList [

	^ notifyList
]

{ #category : #accessing }
OCMockRequestor >> text [

	^ 'I do not really care'
]

{ #category : #accessing }
OCMockRequestor >> textMorph [

	self error: 'Seriously, a Morph?'
]
