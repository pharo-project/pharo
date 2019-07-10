Extension { #name : #SpSliderPresenter }

{ #category : #'*Spec2-Examples' }
SpSliderPresenter class >> example [
	<sampleInstance>
	| slider |
	slider := self new.
	slider
		min: 0;
		max: 100;
		whenValueChangedDo: [ :value | slider label: value asString ].
	slider openWithSpec.
	^ slider 
]
