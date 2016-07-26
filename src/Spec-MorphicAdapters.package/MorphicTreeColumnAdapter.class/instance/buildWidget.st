factory
buildWidget
	^ SpecTreeColumn new
		resizable: self resizable;
		startWidth: self initialWidth;
		setHeaderButtonLabel: self headerLabel
			font: self headerFont
			icon: self headerIcon
			target: self
			actionSelector: self performHeaderActionSelector;
		rowMorphGetSelector: self displayBlock