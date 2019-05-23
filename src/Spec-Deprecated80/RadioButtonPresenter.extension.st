Extension { #name : #RadioButtonPresenter }

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> actionWhenActivatedHolder [
	
	self deprecated: 'It breaks the encapsulation'.
	
	^ nil
]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> actionWhenDeactivatedHolder [
	
	self deprecated: 'It breaks encapsulation'.
	
	^ nil
]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> canDeselectByClick [
	"Return true clicking on a selected radio button deselects it"
	
	self deprecated: 'It is not clear what option to select if deselected'.

	^ false
]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> canDeselectByClick: aBoolean [
	"Set if clicking on a selected radio button can deselect it"
	
	self deprecated: 'It is not clear what option to select if deselected'.	
	
	
]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> privateSetState: aBoolean [
	"Should only be used by RadioButtonGroup"
	
	self deprecated: 'Do not use it'.
	
	state := aBoolean.
	self changed: #state
]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> stateHolder [

	self deprecated: 'It breaks encapsulation'.
	
	^ state
]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> whenCanDeselectByClickChanged: aBlock [
	self deprecated: 'It is not clear what option to select if deselected'.

]

{ #category : #'*Spec-Deprecated80' }
RadioButtonPresenter >> whenCanDeselectByClickChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	self deprecated: 'It is not clear what option to select if deselected'.

]
