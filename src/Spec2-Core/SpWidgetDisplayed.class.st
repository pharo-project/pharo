"
I am raised when a widget has been already shown in the screen
"
Class {
	#name : #SpWidgetDisplayed,
	#superclass : #Announcement,
	#instVars : [
		'presenter',
		'widget'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #accessing }
SpWidgetDisplayed >> presenter [
	^ presenter
]

{ #category : #accessing }
SpWidgetDisplayed >> presenter: anObject [
	presenter := anObject
]

{ #category : #accessing }
SpWidgetDisplayed >> widget [
	^ widget
]

{ #category : #accessing }
SpWidgetDisplayed >> widget: anObject [
	widget := anObject
]
