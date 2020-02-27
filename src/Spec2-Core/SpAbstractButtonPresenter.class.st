Class {
	#name : #SpAbstractButtonPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#icon => SpObservableSlot',
		'#label => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #private }
SpAbstractButtonPresenter >> addShortcutTipFor: aString [

	^ self 
		addShortcutTipFor: self shortcutCharacter
		on: aString
]

{ #category : #private }
SpAbstractButtonPresenter >> addShortcutTipFor: aCharacter on: aString [
	| index text |

	self flag: #REMOVE. "This is morphic specific"
	aString ifNil: [ ^ nil ].
	text := aString asText.
	aCharacter ifNil: [ ^ text ].
	index := aString asLowercase indexOf: aCharacter asLowercase.
	index isZero
		ifTrue: [ ^ text ].

	^ text
		addAttribute: TextEmphasis underlined from: index to: index;
		yourself
]

{ #category : #api }
SpAbstractButtonPresenter >> icon [
	^ icon
]

{ #category : #api }
SpAbstractButtonPresenter >> icon: anIcon [
	icon := anIcon
]

{ #category : #api }
SpAbstractButtonPresenter >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpAbstractButtonPresenter >> initialize [

	super initialize.
	
	label := ''.
	
	self whenLabelChangedDo: [ self changed: #label ].
	self whenIconChangedDo: [ self changed: #label ].

]

{ #category : #morphic }
SpAbstractButtonPresenter >> label [
	^ self addShortcutTipFor: label
]

{ #category : #api }
SpAbstractButtonPresenter >> label: aStringOrImageMorph [
	label := aStringOrImageMorph
]

{ #category : #private }
SpAbstractButtonPresenter >> shortcutCharacter [

	^ nil
]

{ #category : #'api-events' }
SpAbstractButtonPresenter >> whenIconChangedDo: aBlock [
	self property: #icon whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractButtonPresenter >> whenLabelChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #label whenChangedDo: aBlock
]
