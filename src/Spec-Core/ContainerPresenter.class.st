"
I am a model for a container. My purpose is to hold multiple subwidgets to glue them together.


I should not be used directly
"
Class {
	#name : #ContainerPresenter,
	#superclass : #AbstractWidgetPresenter,
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
ContainerPresenter class >> adapterName [

	^ #ContainerAdapter
]

{ #category : #'api-focus' }
ContainerPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"

	^ keyStrokesForNextFocusHolder value
]

{ #category : #'api-focus' }
ContainerPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"

	^ keyStrokesForPreviousFocusHolder value
]

{ #category : #testing }
ContainerPresenter >> isSpecContainer [
	^ true
]
