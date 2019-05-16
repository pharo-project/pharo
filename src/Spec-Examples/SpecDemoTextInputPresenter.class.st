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
		'labelEncrypted',
		'fieldEncrypted',
		'fieldPlaceholderText',
		'labelPlaceholderText'
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
				row add: #labelPlaceholderText right: labelsWidth.
				row add: #fieldPlaceholderText ]
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
SpecDemoTextInputPresenter >> fieldNormal [
	^ fieldNormal
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldNormal: anObject [
	fieldNormal := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldPlaceholderText [
	^ fieldPlaceholderText
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> fieldPlaceholderText: anObject [
	fieldPlaceholderText := anObject
]

{ #category : #initialization }
SpecDemoTextInputPresenter >> initializeWidgets [
	labelNormal := self newLabel label: 'Normal:'.
	fieldNormal := self newTextInput.
	
	labelDisabled := self newLabel label: 'Disabled:'.
	fieldDisabled := self newTextInput enabled: false.
	
	labelPlaceholderText := self newLabel label: 'Placeholder:'.
	fieldPlaceholderText := self newTextInput placeholder: 'Placeholder text'.
	
	labelEncrypted := self newLabel label: 'Password:'.
	fieldEncrypted := self newTextInput
		text: 'Password';
		bePassword
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
SpecDemoTextInputPresenter >> labelNormal [
	^ labelNormal
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelNormal: anObject [
	labelNormal := anObject
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelPlaceholderText [
	^ labelPlaceholderText
]

{ #category : #accessing }
SpecDemoTextInputPresenter >> labelPlaceholderText: anObject [
	labelPlaceholderText := anObject
]
