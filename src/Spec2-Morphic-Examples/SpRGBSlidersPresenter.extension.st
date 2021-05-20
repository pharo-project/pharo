Extension { #name : #SpRGBSlidersPresenter }

{ #category : #'*Spec2-Morphic-Examples' }
SpRGBSlidersPresenter class >> example [
	<sampleInstance>
	| example |
	example := self new
		extent: 300 @ 200;
		openWithSpec;
		yourself.
	example withWindowDo: [ :window | window title: 'RGBSliders example' ].
	^ example
]
