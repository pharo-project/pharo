example
exampleWithoutOptions
	"
	self exampleWithoutOptions
	"

	^ self new
		showOptions: false;
		leftText: 'Old text';
		rightText: 'New text';
		openWithSpec
