"
A SpecLayoutAddWithSpec is an action representing an add in the spec layout with a spec selector precised
"
Class {
	#name : #SpLayoutAddWithSpec,
	#superclass : #SpLayoutAdd,
	#instVars : [
		'specSelector'
	],
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpLayoutAddWithSpec class >> subwidget: subwidget spec: aSpecSelector layoutFrame: aLayoutFrame [

	^ self new
		subwidget: subwidget 
		spec: aSpecSelector 
		layoutFrame:  aLayoutFrame;
		yourself
]

{ #category : #accessing }
SpLayoutAddWithSpec >> specSelector [
	^ specSelector
]

{ #category : #accessing }
SpLayoutAddWithSpec >> specSelector: aSymbol [
	specSelector := aSymbol
]

{ #category : #'instance creation' }
SpLayoutAddWithSpec >> subwidget: sub spec: aSpecSelector layoutFrame: aFrameLayout [
	self subwidget: sub.
	self specSelector: aSpecSelector.
	self layoutFrame: aFrameLayout
]

{ #category : #'instance creation' }
SpLayoutAddWithSpec >> subwidgetArguments [
	^ self subwidget isArray
		ifTrue: [ #(presenter) , self subwidget , {#retrieveSpec:. self specSelector} ]
		ifFalse: [ {#presenter. self subwidget. #retrieveSpec:. self specSelector} ]
]
