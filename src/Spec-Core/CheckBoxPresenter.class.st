"
A CheckBoẍPresenteris a spec presenter for Checkbox.
See AbstractFormButtonPresenter

self example

You can also have a look at CheckBoxExample for a full example of how to use them.

I provide the following methods
todo
- labelOnLeft and labelOnRight
- labelClickable
"
Class {
	#name : #CheckBoxPresenter,
	#superclass : #AbstractFormButtonPresenter,
	#category : #Spec-Core-Widgets
}

{ #category : #specs }
CheckBoxPresenter class >> adapterName [

	^ #CheckBoxAdapter
]

{ #category : #specs }
CheckBoxPresenter class >> defaultSpec [
	<spec>
	
	^ #(CheckBoxAdapter
		adapt: #(model))
]

{ #category : #specs }
CheckBoxPresenter class >> title [

	^ 'Checkbox Button'
]

{ #category : #api }
CheckBoxPresenter >> enabled: aBoolean [
	"<api: #boolean getter: #enabled registration: #whenEnabledChanged:>"
	"Set if the checkbox is enabled (clickable)"

	super enabled: aBoolean.
	self changed: #enabled.

]

{ #category : #api }
CheckBoxPresenter >> labelOnLeft [

	^ self changed: #labelOnLeft with: #()
]

{ #category : #api }
CheckBoxPresenter >> labelOnRight [

	^ self changed: #labelOnRight with: #()
]
