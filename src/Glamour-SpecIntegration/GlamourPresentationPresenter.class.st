"
I am a Spec ComposableModel for all Glamour presentations (GLMPresentation subclasses.) By default I open a GLMPharoScriptPresentation, a Smalltalk code snippet editor.

For examples see my class side.

"
Class {
	#name : #GlamourPresentationPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'presentation',
		'glmPres'
	],
	#category : #'Glamour-SpecIntegration'
}

{ #category : #specs }
GlamourPresentationPresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed add: #presentation
]

{ #category : #example }
GlamourPresentationPresenter class >> exampleGLMPharoScript [
	<sampleInstance>
	^ GlamourPresentationPresenter new openWithSpec.
]

{ #category : #accessing }
GlamourPresentationPresenter >> glmPres [

	glmPres ifNil: [ glmPres := GLMPharoScriptPresentation new ].
	^ glmPres
]

{ #category : #initialization }
GlamourPresentationPresenter >> initializeWidgets [
	"Should actually do nothing at all.
	This is because we cannot create the morph and later set the presentation.
	So we will do all of this in the presentation: accessor.
	"

]

{ #category : #accessing }
GlamourPresentationPresenter >> presentation [

	presentation
		ifNil: [ | morph |

			morph := GLMMorphicRenderer new render: self glmPres.
			morph hResizing: #spaceFill.
			morph vResizing: #spaceFill.
			presentation := MorphicGenericAdapter morph: morph
			].

	^ presentation
]

{ #category : #accessing }
GlamourPresentationPresenter >> presentation: anObject [
	presentation := anObject
]

{ #category : #api }
GlamourPresentationPresenter >> presentationClass: aGLMCompositePresentation startOn: anObject [ 
	glmPres := aGLMCompositePresentation new startOn: anObject
]
