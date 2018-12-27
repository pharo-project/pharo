"
A SpecLayoutAddWithSpec is an action representing an add in the spec layout with a spec selector precised
"
Class {
	#name : #SpecLayoutAddWithSpec,
	#superclass : #SpecLayoutAdd,
	#instVars : [
		'specSelector'
	],
	#category : #'Spec-Layout-Actions'
}

{ #category : #'instance creation' }
SpecLayoutAddWithSpec class >> subwidget: subwidget spec: aSpecSelector layoutFrame: aLayoutFrame [

	^ self new
		subwidget: subwidget 
		spec: aSpecSelector 
		layoutFrame:  aLayoutFrame;
		yourself
]

{ #category : #accessing }
SpecLayoutAddWithSpec >> specSelector [
	^ specSelector
]

{ #category : #accessing }
SpecLayoutAddWithSpec >> specSelector: aSymbol [
	specSelector := aSymbol
]

{ #category : #'instance creation' }
SpecLayoutAddWithSpec >> subwidget: sub spec: aSpecSelector layoutFrame: aFrameLayout [
	self subwidget: sub.
	self specSelector: aSpecSelector.
	self layoutFrame: aFrameLayout
]

{ #category : #'instance creation' }
SpecLayoutAddWithSpec >> subwidgetArguments [
	^ self subwidget isArray
		ifTrue: [ #(presenter) , self subwidget , {#retrieveSpec:. self specSelector} ]
		ifFalse: [ {#presenter. self subwidget. #retrieveSpec:. self specSelector} ]
]
