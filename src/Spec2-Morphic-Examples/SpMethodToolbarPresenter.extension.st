Extension { #name : #SpMethodToolbarPresenter }

{ #category : #'*Spec2-Morphic-Examples' }
SpMethodToolbarPresenter class >> example [
	<sampleInstance>
	| example |
	example := SpMethodToolbarPresenter new.
	example
		method: SpButtonPresenter >> #state:;
		method: nil;
		extent: 550 @ 100;
		openWithSpec.
		^example 
]
