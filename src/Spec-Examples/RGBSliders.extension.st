Extension { #name : #RGBSliders }

{ #category : #'*Spec-Examples' }
RGBSliders class >> example [
	<sampleInstance>
	| example |
	example := self new
		extent: 300 @ 200;
		openDialogWithSpec;
		yourself.
	example withWindowDo: [ :window | window title: 'RGBSliders example' ].
	^ example
]
