Class {
	#name : #CBCriticLocationBookPage,
	#superclass : #CBAbstractNoteBookPage,
	#category : #'Tool-CriticBrowser-utilities-tabContent'
}

{ #category : #visiting }
CBCriticLocationBookPage class >> acceptBlock [
]

{ #category : #accessing }
CBCriticLocationBookPage class >> layoutSpec [
	^ #criticLocationBookPage
]

{ #category : #accessing }
CBCriticLocationBookPage class >> provider: aCriticBrowser [
	aCriticBrowser
		criticLocationBookPage:
			((aCriticBrowser instantiate: CodePresenter new)
				acceptBlock: [ :text | 
					| selectedItem |
					selectedItem := aCriticBrowser criticsPresenter selectedItem
						sourceAnchor entity.
					selectedItem isCompiledMethod 	"and: [ rule isTransformationRule not ]"
						ifTrue: [ selectedItem methodClass compile: text ]
						ifFalse: [ Smalltalk compiler evaluate: text ] ])
]

{ #category : #accessing }
CBCriticLocationBookPage class >> title [
	^ 'Critic location'
]
