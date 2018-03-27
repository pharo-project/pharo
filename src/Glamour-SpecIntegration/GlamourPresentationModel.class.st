"
I am a Spec ComposableModel for all Glamour presentations (GLMPresentation subclasses.) By default I open a GLMPharoScriptPresentation, a Smalltalk code snippet editor.

For examples see my class side.

"
Class {
	#name : #GlamourPresentationModel,
	#superclass : #ComposableModel,
	#instVars : [
		'presentation',
		'glmPres'
	],
	#category : #'Glamour-SpecIntegration'
}

{ #category : #specs }
GlamourPresentationModel class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed add: #presentation
]

{ #category : #example }
GlamourPresentationModel class >> exampleGLMExamples [
	<sampleInstance>
	"open the Glamour Examples Browser"
	|ui|
	ui := GlamourPresentationModel new.
	ui presentationClass: GLMExamplesBrowser startOn: GLMBasicExamples.
	^ ui openWithSpec
]

{ #category : #example }
GlamourPresentationModel class >> exampleGLMPharoScript [
	<sampleInstance>
	^ GlamourPresentationModel new openWithSpec.
]

{ #category : #accessing }
GlamourPresentationModel >> glmPres [
	glmPres isNil ifTrue:[glmPres := GLMPharoScriptPresentation new].
	^ glmPres 
]

{ #category : #initialization }
GlamourPresentationModel >> initializeWidgets [
	"Should actually do nothing at all.
	This is because we cannot create the morph and later set the presentation.
	So we will do all of this in the presentation: accessor.
	"

]

{ #category : #accessing }
GlamourPresentationModel >> presentation [
	presentation isNil ifTrue:[
		| morph |
		morph := GLMMorphicRenderer new render: self glmPres.
		morph hResizing: #spaceFill.
		morph vResizing: #spaceFill.
		presentation := MorphicGenericAdapter morph: morph].

	^ presentation
]

{ #category : #accessing }
GlamourPresentationModel >> presentation: anObject [
	presentation := anObject
]

{ #category : #api }
GlamourPresentationModel >> presentationClass: aGLMCompositePresentation startOn: anObject [ 
	glmPres := aGLMCompositePresentation new startOn: anObject
]
