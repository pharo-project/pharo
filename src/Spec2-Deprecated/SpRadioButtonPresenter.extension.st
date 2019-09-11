Extension { #name : #SpRadioButtonPresenter }

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> actionWhenActivatedHolder [
	
	self deprecated: 'It breaks the encapsulation'.
	
	^ nil
]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> actionWhenDeactivatedHolder [
	
	self deprecated: 'It breaks encapsulation'.
	
	^ nil
]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> canDeselectByClick [
	"Return true clicking on a selected radio button deselects it"
	
	self deprecated: 'It is not clear what option to select if deselected'.

	^ false
]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> canDeselectByClick: aBoolean [
	"Set if clicking on a selected radio button can deselect it"
	
	self deprecated: 'It is not clear what option to select if deselected'.	
	
	
]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> privateSetState: aBoolean [
	"Should only be used by RadioButtonGroup"
	
	self deprecated: 'Do not use it'.
	
	state := aBoolean.
	self changed: #state
]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> stateHolder [

	self deprecated: 'It breaks encapsulation'.
	
	^ state
]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> whenCanDeselectByClickChanged: aBlock [
	self deprecated: 'It is not clear what option to select if deselected'.

]

{ #category : #'*Spec2-Deprecated' }
SpRadioButtonPresenter >> whenCanDeselectByClickChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	self deprecated: 'It is not clear what option to select if deselected'.

]
