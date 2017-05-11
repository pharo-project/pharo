public
newApplyPreviewLog

	| events |
	events := self entries flatCollect: [:each | 
		each content previewedApplyEvents ].
	
	^ self newPreviewLogFor: events
