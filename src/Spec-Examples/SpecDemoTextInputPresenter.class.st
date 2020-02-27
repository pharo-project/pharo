"
This demo shows differenty styles of text inputs.
"
Class {
	#name : #SpecDemoTextInputPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'labelNormal',
		'fieldNormal',
		'labelDisabled',
		'fieldDisabled',
		'labelGhostText',
		'filedGhostText',
		'fieldGhostText',
		'labelEncrypted',
		'fieldEncrypted'
	],
	#category : #'Spec-Examples-Demo-TextInput'
}

{ #category : #specs }
SpecDemoTextInputPresenter class >> defaultSpec [

	<spec: #default>

	| fontWidth labelsWidth rowHeight |
	fontWidth := (StandardFonts defaultFont widthOfString: 'M').
	labelsWidth := fontWidth * 8.
	rowHeight := self inputTextHeight.
	
	^ SpecLayout composed
		newColumn: [ :col | 

			col newRow: [ :row | 
				row add: #labelNormal right: labelsWidth.
				row add: #fieldNormal ]
			height: rowHeight.
		
			col newRow: [ :row | 
				row add: #labelDisabled right: labelsWidth.
				row add: #fieldDisabled ]
			height: rowHeight.

			col newRow: [ :row | 
				row add: #labelGhostText right: labelsWidth.
				row add: #fieldGhostText ]
			height: rowHeight.

			col newRow: [ :row | 
				row add: #labelEncrypted right: labelsWidth.
				row add: #fieldEncrypted ]
			height: rowHeight.

		"fill the rest of the space"
		col newRow: [ :row |  ] ]; 
			
		yourself.	
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldDisabled [
	^ fieldDisabled
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldDisabled: anObject [
	fieldDisabled := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldEncrypted [
	^ fieldEncrypted
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldEncrypted: anObject [
	fieldEncrypted := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldGhostText [
	^ fieldGhostText
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldGhostText: anObject [
	fieldGhostText := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldNormal [
	^ fieldNormal
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldNormal: anObject [
	fieldNormal := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> filedGhostText [
	^ filedGhostText
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> filedGhostText: anObject [
	filedGhostText := anObject
]

{ #category : #initialization }
SpecDemoTextInputPresenter >> initializeWidgets [

	labelNormal := self newLabel label: 'normal:'.
	fieldNormal := self newTextInput.

	labelDisabled := self newLabel label: 'disabled:'.
	fieldDisabled := self newTextInput enabled: false.

	labelGhostText := self newLabel label: 'ghost text:'.
	fieldGhostText := self newTextInput ghostText: 'ghost text'.
	
	labelEncrypted := self newLabel label: 'encrypted:'.
	fieldEncrypted := self newTextInput text: 'password'; encrypted: true.

]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelDisabled [
	^ labelDisabled
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelDisabled: anObject [
	labelDisabled := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelEncrypted [
	^ labelEncrypted
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelEncrypted: anObject [
	labelEncrypted := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelGhostText [
	^ labelGhostText
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelGhostText: anObject [
	labelGhostText := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelNormal [
	^ labelNormal
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelNormal: anObject [
	labelNormal := anObject
]
