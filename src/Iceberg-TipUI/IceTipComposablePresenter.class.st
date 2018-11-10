"
I'm a generic presenter of IceTip UI elements.
"
Class {
	#name : #IceTipComposablePresenter,
	#superclass : #ComposablePresenter,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceTipComposablePresenter class >> iconNamed: aSymbol [
	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : #specs }
IceTipComposablePresenter class >> title [
	^ self subclassResponsibility
]

{ #category : #adding }
IceTipComposablePresenter >> addShortcutsTo: aWidget [
	self enableCommanderShortcutsIn: aWidget
]

{ #category : #accessing }
IceTipComposablePresenter >> contextClass [
	"default context. This needs to be overriden for using presenters"
	^ IceTipContext
]

{ #category : #accessing }
IceTipComposablePresenter >> contextSelection [
	"to be overriden by my users"
	^ nil
]

{ #category : #'private factory' }
IceTipComposablePresenter >> createCommandContext [
	^ self newContextWithSelection: self contextSelection
]

{ #category : #'private factory' }
IceTipComposablePresenter >> createCommandContextForSelection: selectedItems [
	^ self newContextWithSelection: selectedItems
]

{ #category : #adding }
IceTipComposablePresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"	
		
	^ { Character tab asKeyCombination }
]

{ #category : #adding }
IceTipComposablePresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"
	
	^ { Character tab shift asKeyCombination }
]

{ #category : #accessing }
IceTipComposablePresenter >> iconNamed: aSymbol [
	^ self class iconNamed: aSymbol
]

{ #category : #'private factory' }
IceTipComposablePresenter >> newContext [
	^ self contextClass for: self
]

{ #category : #'private factory' }
IceTipComposablePresenter >> newContextWithSelection: anObject [
	^ self newContext
]
