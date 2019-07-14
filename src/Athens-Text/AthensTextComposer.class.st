"
Compose text
"
Class {
	#name : #AthensTextComposer,
	#superclass : #Object,
	#instVars : [
		'lines',
		'text'
	],
	#category : #'Athens-Text'
}

{ #category : #'instance creation' }
AthensTextComposer class >> scan: aText for: scanner [
	^ self new scan: aText for: scanner
]

{ #category : #accessing }
AthensTextComposer >> lines [
	^ lines
]

{ #category : #rendering }
AthensTextComposer >> renderOn: aCanvas [

	| renderer |
	
	renderer := AthensTextRenderer new.
	renderer render: text lines: lines on: aCanvas.

]

{ #category : #'private - scanning' }
AthensTextComposer >> scan: aText for: scanner [
	" scan text from starting index,
	answer the index of first character for potential next line"
	| top |	

	text := aText.
	scanner text: text.

	"edge case, text is empty "
	text isEmpty ifTrue: [ ^ scanner ].
	
	scanner newLine: 1.

	text runs withStartStopAndValueDo: [:start :stop :values |
		
		scanner setAttributes: values.
		start to: stop do: [:i |  | ch |
			ch := text at: i.
			ch = Character cr 
				ifTrue: [ 
					scanner endLine: i - 1.
					scanner newLine: i + 1 ]
				ifFalse: [
					scanner addCharacter: i.
				]
			]
	].
	scanner endLine: (text size).

	lines := scanner lines.

	"set the lines top offset"
	top := 0.
	lines do: [:line |
		line top: top.
		top := top + line height ].
	

]
