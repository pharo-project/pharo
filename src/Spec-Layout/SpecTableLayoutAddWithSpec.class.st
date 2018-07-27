"
A SpecLayoutAddWithSpec is an action representing an add in the spec table layout with a spec selector precised

"
Class {
	#name : #SpecTableLayoutAddWithSpec,
	#superclass : #SpecTableLayoutAdd,
	#category : #'Spec-Layout-Actions'
}

{ #category : #'instance creation' }
SpecTableLayoutAddWithSpec class >> subwidget: sub spec: aSpecSelector [
	^ self new 
		subwidget: sub 
		spec: aSpecSelector
]

{ #category : #'instance creation' }
SpecTableLayoutAddWithSpec >> subwidget: sub spec: aSpecSelector [
	self subwidget: (sub isArray 
						ifTrue: [ #(model), sub , {#retrieveSpec:. aSpecSelector} ]
						ifFalse: [ {#model. sub . #retrieveSpec: . aSpecSelector } ]).
	
	self generateArguments
]
