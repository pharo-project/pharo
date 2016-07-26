specs
defaultSpec

	^ SpecLayout composed
		newRow: #searchModel height: self toolbarHeight;
		add: #listModel top: self toolbarHeight;
		yourself