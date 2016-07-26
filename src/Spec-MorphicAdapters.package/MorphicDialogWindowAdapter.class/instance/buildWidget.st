factory
buildWidget

	^ SpecDialogWindow new
			specWidget: self contentMorph;
			setToolbarFrom: self toolbarBlock;
			model: self model;
			initialize;
			yourself