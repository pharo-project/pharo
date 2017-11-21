"
A SpecFocusOrder manage the path of focus between widgets.

I use a collection (presenters) to keep widgets I managed. When I reach the end of the collection, I try to give the focus at the parent in the hierarchy, if it is nil I loop.

I provide the following methods
- add: and addLast: to add a widget in my collection
- giveFocusToNextFrom:for: and giveFocusToPreviousFrom:for: to make me change the focus, the first argument is presenter who leave the focus and the second is the current model in the hierarchy.
- ifEmpty:ifNotEmpty: and ifNotEmpty: are send in my collection.
- presenters is the getter of my collection.
- removeAll to remove all my widgets.

My super class is not OrderedCollection because it could be a problem for the devellopement of another behaviour.
"
Class {
	#name : #SpecFocusOrder,
	#superclass : #Object,
	#instVars : [
		'presenters'
	],
	#category : #Spec-Core-Support
}

{ #category : #accessing }
SpecFocusOrder >> add: aPresenter [
	presenters add: aPresenter
]

{ #category : #accessing }
SpecFocusOrder >> addLast: aPresenter [
	presenters addLast: aPresenter
]

{ #category : #operating }
SpecFocusOrder >> giveFocusToNextFrom: aPresenter for: aModel [
	"aPresenter is the object what currently get the focus, aModel is the object use to call the good parent in a complex hierarchy"

	| focus |
	focus := self presenters.
	(focus includes: aPresenter)
		ifTrue: [ | index next |
			index := (focus indexOf: aPresenter) + 1.
			index <= focus size
				ifTrue: [ next := focus at: index.
					next takeKeyboardFocus.
					^ true ] ].
	^ aModel owner
		ifNil: [ | next |
			next := focus at: 1 ifAbsent: [ ^ false ].
			next takeKeyboardFocus.
			true ]
		ifNotNil: [ aModel owner giveFocusToNextFrom: aModel.
			true ]	"loop if there is no parent, else give it the focus"
]

{ #category : #operating }
SpecFocusOrder >> giveFocusToPreviousFrom: aPresenter for: aModel [
	| focus |
	focus := self presenters.
	(focus includes: aPresenter)
		ifTrue: [ | index previous |
			index := (focus indexOf: aPresenter) - 1.
			index > 0
				ifTrue: [ previous := focus at: index.
					previous takeLastKeyboardFocus.
					^ true ] ].
	^ aModel owner
		ifNil: [ | next |
			next := focus at: focus size ifAbsent: [ ^ false ].
			next takeLastKeyboardFocus.
			true ]
		ifNotNil: [ aModel owner giveFocusToPreviousFrom: aModel.
			true ]	" I loop "
]

{ #category : #'to clean later' }
SpecFocusOrder >> ifEmpty: aBlock ifNotEmpty: aBlock2 [
	^ presenters
		ifEmpty: aBlock
		ifNotEmpty: aBlock2
]

{ #category : #'to clean later' }
SpecFocusOrder >> ifNotEmpty: aBlock [
	^ presenters ifNotEmpty: aBlock
]

{ #category : #initialization }
SpecFocusOrder >> initialize [

	super initialize.

	presenters := OrderedCollection new.
]

{ #category : #accessing }
SpecFocusOrder >> presenters [
	^ presenters
]

{ #category : #'to clean later' }
SpecFocusOrder >> removeAll [
^ presenters removeAll
]
