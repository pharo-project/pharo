Extension { #name : #SpMethodToolbar }

{ #category : #'*Spec2-Morphic-Examples' }
SpMethodToolbar class >> example [
	<sampleInstance>
	| example |
	example := SpMethodToolbar new.
	example
		method: SpButtonPresenter >> #state:;
		method: nil;
		extent: 550 @ 100;
		openWithSpec.
		^example 
]
