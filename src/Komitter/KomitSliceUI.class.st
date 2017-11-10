"
I am a UI used to retrieve a Slice title using its issue number
"
Class {
	#name : #KomitSliceUI,
	#superclass : #ComposablePresenter,
	#instVars : [
		'fork',
		'issueNumber',
		'loadingWidget',
		'titleWidget',
		'valid'
	],
	#category : #Komitter-UI
}

{ #category : #specs }
KomitSliceUI class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :col |
			col
				newRow: [ :row |
					row
						add: #issueNumber width: 90;
						add: #titleWidget ] height: self inputTextHeight;
				newRow: [ :row | ] ];
		yourself
]

{ #category : #initialization }
KomitSliceUI >> encodeTitle: aTitle [
	| allowedCharacters |
	
	allowedCharacters := ($A to: $Z)
				, ($a to: $z)
				, ($0 to: $9)
				, (Array with: $-).
	^ String streamContents: [ :stream | 			
		aTitle
			do: [ :character | (' 	_' includes: character)
				ifTrue: [ stream << '-' ]
				ifFalse: [ (allowedCharacters includes: character)
						ifTrue: [ stream << character ] ] ] ]
]

{ #category : #api }
KomitSliceUI >> initialExtent [

	^ 600@100
]

{ #category : #initialization }
KomitSliceUI >> initialize [

	super initialize.

	valid := false asValueHolder
]

{ #category : #initialization }
KomitSliceUI >> initializeDialogWindow: aWindow [

	valid whenChangedDo: [ :b | aWindow toolbar okButton enabled: b ].
	aWindow toolbar okButton enabled: false.
	issueNumber 
		bindKeyCombination: Character cr control unix | $m control mac
		toAction: [ self valid ifTrue: [ aWindow triggerOkAction ] ]
]

{ #category : #initialization }
KomitSliceUI >> initializeWidgets [

	issueNumber := self newTextInput.
	titleWidget := self newTextInput.

	titleWidget
		enabled: false;
		ghostText: 'Title'.
		
	issueNumber ghostText: 'Issue number'.

	issueNumber 
		autoAccept: true;
		acceptBlock: [ :text |
			fork ifNotNil: [ fork terminate ].
			loadingWidget loading.
			self valid: false.
			fork := [ | response |
				response := ZnEasy client
				url: 'http://tracker.pharo.org/issues/name/', text asString;
				get;
				response.
				self setTitleFrom: response ] fork ].
	self focusOrder
		add: issueNumber
]

{ #category : #accessing }
KomitSliceUI >> issueNumber [
	^ issueNumber
]

{ #category : #api }
KomitSliceUI >> issueNumberText [

	^ issueNumber text
]

{ #category : #accessing }
KomitSliceUI >> loadingWidget [
	^ loadingWidget
]

{ #category : #private }
KomitSliceUI >> setTitleFrom: aZnResponse [

	(aZnResponse isError or: [ aZnResponse entity isNil ])
		ifTrue: [ 
			titleWidget text: 'Error !'.
			loadingWidget error ]
		ifFalse: [ 
			titleWidget text: (self encodeTitle: aZnResponse entity string).
			self valid: true.
			loadingWidget ok ]
]

{ #category : #api }
KomitSliceUI >> title [

	^ 'Retrieve Slice title'
]

{ #category : #api }
KomitSliceUI >> titleText [

	^ titleWidget text
]

{ #category : #accessing }
KomitSliceUI >> titleWidget [
	^ titleWidget
]

{ #category : #api }
KomitSliceUI >> valid [

	^ valid value
]

{ #category : #api }
KomitSliceUI >> valid: aBoolean [

	valid value: aBoolean
]
