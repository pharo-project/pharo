"
I am raised when a widget will be built. 

I may be used in situations when you need to change the content of the context menu before it will be displayed and so on.
"
Class {
	#name : #SpWidgetWillBeBuilt,
	#superclass : #Announcement,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #'instance creation' }
SpWidgetWillBeBuilt class >> model: model [

	^ self new
		model: model;
		yourself
]

{ #category : #accessing }
SpWidgetWillBeBuilt >> model [
	^ model
]

{ #category : #accessing }
SpWidgetWillBeBuilt >> model: anObject [
	model := anObject
]
