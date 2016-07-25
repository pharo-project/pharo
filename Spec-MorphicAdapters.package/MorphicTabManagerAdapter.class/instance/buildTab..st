factory
buildTab: aTab
	^ aTab buildWithSpec
		when: #tabDeleted send: #tabDeleted: to: self;
		yourself