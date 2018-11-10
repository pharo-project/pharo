"
I'm a filter function that links with a text presenter. 
"
Class {
	#name : #IceTipTreeLinkFilterFunction,
	#superclass : #IceTipTreeFilterFunction,
	#instVars : [
		'textField'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #private }
IceTipTreeLinkFilterFunction >> showFilterFieldFromKeystrokeEvent: anEvent [
	"nothing tho show, we will use a linked text presenter. 
	 intead, transmit the focus (and key) to the linked presenter"
	| text |

	text := anEvent keyCharacter asString.
	self textField takeKeyboardFocus.
	self textField text: self textField text, text.
	self flag: #hack. "I'm going directly to the widget because I need 
	control over the internals to position the cursor."
	self textField textArea cursorEnd: anEvent
]

{ #category : #accessing }
IceTipTreeLinkFilterFunction >> showWidget [
	"nothing tho show, we will use a linked text presenter"
]

{ #category : #accessing }
IceTipTreeLinkFilterFunction >> textField [
	^ textField
]

{ #category : #accessing }
IceTipTreeLinkFilterFunction >> textField: aTextPresenter [
	textField := aTextPresenter.
	textField whenTextChanged: [ :ann | self textUpdated: ann ]
]

{ #category : #accessing }
IceTipTreeLinkFilterFunction >> textUpdated: anAnnouncement [
	self filterWith: self textField getText
]
