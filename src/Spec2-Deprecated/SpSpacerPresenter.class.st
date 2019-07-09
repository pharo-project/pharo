"
Description
--------------------

I am a simple presenter to add spacers to spec layouts.
"
Class {
	#name : #SpSpacerPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpSpacerPresenter class >> adapterName [

	^ #SpacerAdapter
]

{ #category : #deprecation }
SpSpacerPresenter class >> isDeprecated [
	"Was needed only for old layouts."

	^ true
]
