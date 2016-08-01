initialization
initializePresenter
	button action: [ input text: textToReset ].
	input whenTextChanged: [:text | label label: text].
	check
		whenActivatedDo: [ self needRebuild: false.
			self buildWithSpecLayout: self class defaultSpec ];
		whenDeactivatedDo: [ self needRebuild: false.
			self buildWithSpecLayout: self class bottomLayout ]