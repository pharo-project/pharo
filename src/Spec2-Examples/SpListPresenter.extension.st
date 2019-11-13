Extension { #name : #SpListPresenter }

{ #category : #'*Spec2-Examples' }
SpListPresenter class >> example [
	<sampleInstance>
	^ self new
		items: self environment allClasses;
		openWithSpec;
		yourself
]

{ #category : #'*Spec2-Examples' }
SpListPresenter class >> exampleMultipleSelection [
	<sampleInstance>
	^ self new
		items: self environment allClasses;
		beMultipleSelection;
		openWithSpec;
		yourself
]

{ #category : #'*Spec2-Examples' }
SpListPresenter class >> exampleWithHeaderTitle [
	<sampleInstance>
	^ self new
		items: self environment allClasses;
		headerTitle: 'Title';
		openWithSpec;
		yourself
]

{ #category : #'*Spec2-Examples' }
SpListPresenter class >> exampleWithIcons [
	<sampleInstance>
	^ self new
		icons: [ :e | self iconNamed: #forwardIcon ];
		items: (1 to: 10) asArray;
		openWithSpec;
		yourself
]
