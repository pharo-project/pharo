factory
buildWidget
	^ PluggableSliderMorph new
		model: self;
		getValueSelector: #value;
		setValueSelector: #value:;
		value: self absoluteValue;
		getLabelSelector: #label;
		max: self max;
		min: self min;
		quantum: self quantum;
		setBalloonText: self help;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself