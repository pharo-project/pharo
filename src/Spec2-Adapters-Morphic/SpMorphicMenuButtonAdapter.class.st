Class {
	#name : #SpMorphicMenuButtonAdapter,
	#superclass : #SpMorphicButtonAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'widget API' }
SpMorphicMenuButtonAdapter >> action [ 

	self presenter menu ifNotNil: [ :aValuable |
		aValuable value ifNotNil: [ :aMenu |
			aMenu owner: self presenter.
			self showSubMenu: aMenu ] ].
	
	self widget ifNotNil: [ :m | 
		m takeKeyboardFocus ] 
]

{ #category : #protocol }
SpMorphicMenuButtonAdapter >> askBeforeChanging [

	^ false
]

{ #category : #factory }
SpMorphicMenuButtonAdapter >> defaultIcon [
		
	^ self application iconNamed: #arrowDown
]

{ #category : #'drag and drop' }
SpMorphicMenuButtonAdapter >> dragEnabled [

	^ false
]

{ #category : #'drag and drop' }
SpMorphicMenuButtonAdapter >> dropEnabled [

	^ false
]

{ #category : #'widget API' }
SpMorphicMenuButtonAdapter >> font [
	
	self flag: #REMOVE. "Not used anymore. Font is set with styles"
	^ nil 
]

{ #category : #accessing }
SpMorphicMenuButtonAdapter >> icon [

	^ self presenter icon ifNil: [ self defaultIcon ]
]

{ #category : #factory }
SpMorphicMenuButtonAdapter >> newButton [

	^ PluggableButtonMorph
		on: self 
		getState: nil
		action:  #action 
		label: #label 
		icon: #icon
		menu: nil
]
