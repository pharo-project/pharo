specs
defaultSpec

	^ SpecLayout composed
		newRow: #searchPresenter height: self toolbarHeight;
		add: #listPresenter top: self toolbarHeight;
		yourself