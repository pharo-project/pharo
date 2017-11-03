"
A DialogWindowPresenter is a presenter used to describe a DialogWindow
"
Class {
	#name : #DialogWindowPresenter,
	#superclass : #WindowPresenter,
	#instVars : [
		'contents',
		'toolbar'
	],
	#category : #Spec-Core-Support
}

{ #category : #specs }
DialogWindowPresenter class >> adapterName [

	^ #DialogWindowAdapter
]

{ #category : #specs }
DialogWindowPresenter class >> defaultSpec [
	<spec>
	
	^ #(DialogWindowAdapter
		adapt: #(model))
]

{ #category : #private }
DialogWindowPresenter >> buildWithSpecLayout: aSpec [
	"Build the widget using the spec name provided as argument"
	| widget |
	
	(self spec notNil and: [ self needRebuild not ])
		ifTrue: [ widget := self spec instance ]
		ifFalse: [ 
			contents := self model buildWithSpecLayout: aSpec.
			widget := self privateAdapterFromModel: self withSpec: self defaultSpecSelector.
			contents := nil ].

	self extent ifNotNil: [:ex | 
		(widget respondsTo: #extent:)
			ifTrue: [ widget extent: ex ]].
		
	^ widget
]

{ #category : #api }
DialogWindowPresenter >> cancelAction: aBlock [

	^ self changed: #cancelAction: with: { aBlock }
]

{ #category : #api }
DialogWindowPresenter >> cancelled [

	^ self toolbar cancelled
]

{ #category : #accessing }
DialogWindowPresenter >> contents [

	^ contents
]

{ #category : #initialization }
DialogWindowPresenter >> initialize [

	super initialize.

	toolbar := OkCancelToolbar new asValueHolder.
	toolbar whenChangedDo: [ :t | self changed: #toolbar: with: { t } ]
]

{ #category : #api }
DialogWindowPresenter >> okAction: aBlock [

	^ self changed: #okAction: with: { aBlock }
]

{ #category : #api }
DialogWindowPresenter >> okButtonEnabled: aBoolean [

	self changed: #okButtonEnabled: with: { aBoolean }
]

{ #category : #api }
DialogWindowPresenter >> toolbar [

	^ toolbar value
]

{ #category : #api }
DialogWindowPresenter >> toolbar: aBar [

	toolbar value: aBar
]

{ #category : #api }
DialogWindowPresenter >> triggerCancelAction [

	self changed: #triggerCancelAction with: #()
]

{ #category : #api }
DialogWindowPresenter >> triggerOkAction [

	self changed: #triggerOkAction with: #()
]
