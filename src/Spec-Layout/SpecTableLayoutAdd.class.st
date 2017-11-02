"
A SpecTableLayoutAdd represents the action to add a subwidget into a spec table layout

"
Class {
	#name : #SpecTableLayoutAdd,
	#superclass : #SpecTableLayoutSend,
	#instVars : [
		'subwidget'
	],
	#category : 'Spec-Layout'
}

{ #category : #'instance creation' }
SpecTableLayoutAdd class >> subwidget: aSymbol [
	^ self new 
		subwidget: aSymbol;
		yourself
]

{ #category : #'instance creation' }
SpecTableLayoutAdd >> generateArguments [
	self arguments: {{subwidget}}
]

{ #category : #initialization }
SpecTableLayoutAdd >> initialize [
	super initialize.
	self selector: #add:
]

{ #category : #accessing }
SpecTableLayoutAdd >> subwidget [
	^ subwidget
]

{ #category : #accessing }
SpecTableLayoutAdd >> subwidget: aSpec [
	subwidget := aSpec isSymbol
		ifTrue: [{#model. aSpec}]
		ifFalse: [
			aSpec isCollection
				ifTrue: [
					aSpec isEmpty 
						ifTrue: [ "should raise an error" ]
						ifFalse: [ aSpec first == #model 
										ifTrue: [ aSpec ]
										ifFalse: [ {#model}, aSpec ]]]
				ifFalse: [ 
					"Not a symbol or a collection. We assume it's an object to add directly (like a morph by example)"
					aSpec ]].
		
	self generateArguments
]
