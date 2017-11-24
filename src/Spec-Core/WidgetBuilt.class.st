"
I am raised when a widget has been built
"
Class {
	#name : #WidgetBuilt,
	#superclass : #Announcement,
	#instVars : [
		'model',
		'widget'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #'instance creation' }
WidgetBuilt class >> model: model widget: widget [

	^ self new
		model: model;
		widget: widget;
		yourself
]

{ #category : #accessing }
WidgetBuilt >> model [
	^ model
]

{ #category : #accessing }
WidgetBuilt >> model: anObject [
	model := anObject
]

{ #category : #accessing }
WidgetBuilt >> widget [
	^ widget
]

{ #category : #accessing }
WidgetBuilt >> widget: anObject [
	widget := anObject
]
