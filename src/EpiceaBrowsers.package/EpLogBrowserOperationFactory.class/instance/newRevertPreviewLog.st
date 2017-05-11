public
newRevertPreviewLog

	| events |
	events := self entries reversed flatCollect: [:each | 
		each content asRevertedCodeChange previewedApplyEvents ].

	^ self newPreviewLogFor: events
