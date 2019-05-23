Extension { #name : #DiffPresenter }

{ #category : #'*Spec-Examples' }
DiffPresenter class >> exampleWithOptions [
	"
	self exampleWithOptions
	"
	<sampleInstance>
	^ self new
		showOptions: true;
		leftText: (True >> #and:) sourceCode;
		rightText: (True >> #or:) sourceCode;
		contextClass: True;
		openWithSpec
]

{ #category : #'*Spec-Examples' }
DiffPresenter class >> exampleWithoutOptions [
	"
	self exampleWithoutOptions
	"
	<sampleInstance>
	
	^ self new
		showOptions: false;
		leftText: 'Old text';
		rightText: 'New text';
		openWithSpec

]
