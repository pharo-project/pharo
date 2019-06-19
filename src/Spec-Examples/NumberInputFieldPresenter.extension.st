Extension { #name : #NumberInputFieldPresenter }

{ #category : #'*Spec-Examples' }
NumberInputFieldPresenter class >> example [
	<sampleInstance>
	^ self new
		extent: 300 @ 20;
		placeholder: 'Enter a number';
		minimum: 10;
		maximum: 30;
		climbRate: 2;
		openWithSpec;
		yourself
]
