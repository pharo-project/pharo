"
A  Checkbox button that can be activated/deactivated.

## Examples
- `SpCheckBoxPresenter class>>#example`
-  `SpCheckBoxExample`

See also `AbstractFormButtonPresenter`

"
Class {
	#name : #SpCheckBoxPresenter,
	#superclass : #SpAbstractFormButtonPresenter,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpCheckBoxPresenter class >> adapterName [

	^ #CheckBoxAdapter
]

{ #category : #specs }
SpCheckBoxPresenter class >> title [

	^ 'Checkbox Button'
]

{ #category : #TOREMOVE }
SpCheckBoxPresenter >> labelOnLeft [

	^ self changed: #labelOnLeft with: #()
]

{ #category : #TOREMOVE }
SpCheckBoxPresenter >> labelOnRight [

	^ self changed: #labelOnRight with: #()
]
