Class {
	#name : #SpMenuButtonPresenterTest,
	#superclass : #SpAbstractButtonPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpMenuButtonPresenterTest >> classToTest [

	^ SpMenuButtonPresenter
]

{ #category : #tests }
SpMenuButtonPresenterTest >> testSmokeMenu [
	
	presenter menu: (SpMenuPresenter new
		addGroup: [ :group | group
			addItem: [ :item | item name: 'One' ];
			addItem: [ :item | item name: 'Two' ];
			addItem: [ :item | item name: 'Three' ] ];
		yourself).
	self openInstance
]
