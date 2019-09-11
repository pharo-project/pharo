"
I am raised when a widget has been built with all its children, but not yet shown
"
Class {
	#name : #SpWidgetBuilt,
	#superclass : #Announcement,
	#instVars : [
		'model',
		'widget'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #'instance creation' }
SpWidgetBuilt class >> model: model widget: widget [

	^ self new
		model: model;
		widget: widget;
		yourself
]

{ #category : #accessing }
SpWidgetBuilt >> model [
	^ model
]

{ #category : #accessing }
SpWidgetBuilt >> model: anObject [
	model := anObject
]

{ #category : #accessing }
SpWidgetBuilt >> widget [
	^ widget
]

{ #category : #accessing }
SpWidgetBuilt >> widget: anObject [
	widget := anObject
]
