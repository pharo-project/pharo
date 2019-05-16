Extension { #name : #LinkPresenter }

{ #category : #'*Spec-Examples' }
LinkPresenter class >> example [
	<sampleInstance>
	^ self new
		url: 'https://pharo.org';
		label: 'Pharo website';
		extent: 200 @ 20;
		openWithSpec;
		yourself
]
