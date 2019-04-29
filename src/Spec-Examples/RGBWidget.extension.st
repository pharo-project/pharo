Extension { #name : #RGBWidget }

{ #category : #'*Spec-Examples' }
RGBWidget class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		title: 'RGBWidget exampe';
		extent: 300 @ 250;
		openDialogWithSpec.
	^ example
]
