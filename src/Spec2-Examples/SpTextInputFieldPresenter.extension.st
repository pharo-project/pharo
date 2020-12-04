Extension { #name : #SpTextInputFieldPresenter }

{ #category : #'*Spec2-Examples' }
SpTextInputFieldPresenter class >> example [
	<sampleInstance>

	^ self new
		extent: 300 @ 25;
		placeholder: 'This is a ghost';
		text: 'This is a text';
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpTextInputFieldPresenter class >> examplePassword [

	^ self new
		extent: 300 @ 25;
		bePassword;
		placeholder: 'This is a ghost';
		text: 'shhh';
		openWithSpec
]
