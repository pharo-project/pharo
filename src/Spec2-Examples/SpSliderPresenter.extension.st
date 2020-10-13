Extension { #name : #SpSliderPresenter }

{ #category : #'*Spec2-Examples' }
SpSliderPresenter class >> example [
	<sampleInstance>
	| slider |
	
	^ (slider := self new)
		min: 0;
		max: 100;
		whenValueChangedDo: [ :value | slider label: value asString ];
		openWithSpec
]
