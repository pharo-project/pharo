Class {
	#name : #SpPopoverPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpPopoverPresenterTest >> classToTest [
	
	
	^ SpPopoverPresenter
]

{ #category : #running }
SpPopoverPresenterTest >> initializeTestedInstance [

	presenter presenter: (SpPresenter new 
		layout: (SpBoxLayout newVertical
			add: 'Test Popover';
			add: (presenter newButton label: 'Button');
			yourself);
		yourself)
]

{ #category : #tests }
SpPopoverPresenterTest >> testPopoverTakesExtentFromPresenter [

	self openInstance.
	
	self 
		assert: presenter adapter widget width >= presenter presenter adapter widget width;
		assert: presenter adapter widget height >= presenter presenter adapter widget height.
]
