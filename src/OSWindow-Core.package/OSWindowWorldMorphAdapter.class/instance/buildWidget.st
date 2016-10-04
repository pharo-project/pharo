factory
buildWidget
	^ OSWindowWorldMorphWithModel new
		layoutPolicy: ProportionalLayout new;
		model: model;
		isResizeable: self isResizeable;
		yourself