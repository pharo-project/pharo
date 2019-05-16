Extension { #name : #RGBSliders }

{ #category : #'*Spec-Examples' }
RGBSliders class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		title: 'RGBSliders example';
		extent: 300 @ 200;
		openDialogWithSpec.
	^ example
]
