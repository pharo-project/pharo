Class {
	#name : #SpMenuItemPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpMenuItemPresenterTest >> classToTest [

	^ SpMenuItemPresenter
]

{ #category : #tests }
SpMenuItemPresenterTest >> testIcon [

	presenter icon: (Smalltalk ui icons iconNamed: 'smallCancel').
	self openInstance.
	presenter icon: (Smalltalk ui icons iconNamed: 'smallOk').
	self 
		assert: presenter adapter widgetIcon 
		equals: (Smalltalk ui icons iconNamed: 'smallOk')
]

{ #category : #tests }
SpMenuItemPresenterTest >> testName [

	presenter name: 'Test'.
	self openInstance.
	presenter name: 'Changed'.
	self assert: presenter adapter widgetName equals: 'Changed'
]
