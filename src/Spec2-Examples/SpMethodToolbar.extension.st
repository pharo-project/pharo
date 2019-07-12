Extension { #name : #SpMethodToolbar }

{ #category : #'*Spec2-Examples' }
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
