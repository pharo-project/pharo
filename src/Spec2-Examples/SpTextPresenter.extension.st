Extension { #name : #SpTextPresenter }

{ #category : #'*Spec2-Examples' }
SpTextPresenter class >> example [

	^ self new
		text:
			'This is a test.
If this had been an actual emergency...';
		openWithSpec


]

{ #category : #'*Spec2-Examples' }
SpTextPresenter class >> exampleWithoutEditionContextMenu [
	"This example should show a text editor without the default edition context menu"

	^ self new
		withoutEditionContextMenu;
		text:
			'This is a test.
If this had been an actual emergency...
(If you right click here, no context menu will be shown)';
		openWithSpec


]
