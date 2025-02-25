"
I'm a text attribute used to encode an URL.

TextMorph new 
	newContents: ('Visit Pharo' asText addAttribute: (TextURL new url: 'http://pharo.org')) ;
	openInWindow.
"
Class {
	#name : 'TextURL',
	#superclass : 'TextAction',
	#instVars : [
		'url'
	],
	#category : 'Text-Core-Attributes',
	#package : 'Text-Core',
	#tag : 'Attributes'
}

{ #category : 'evaluating' }
TextURL >> actOnClick: anEvent for: anObject in: paragraph editor: editor [
	self notify: 'Please configure the actOnClickBlock to invoke the computation you need. In Pharo by default there is no external web browser'
]

{ #category : 'testing' }
TextURL >> analyze: aString [

	| list |
	list := super analyze: aString.
	url := list at: 1.
	^ list at: 2
]

{ #category : 'accessing' }
TextURL >> info [
	^ url
]

{ #category : 'accessing' }
TextURL >> url: aString [
	url := aString
]
