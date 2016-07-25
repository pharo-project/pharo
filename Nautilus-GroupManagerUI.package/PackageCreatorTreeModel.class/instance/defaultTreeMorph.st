view
defaultTreeMorph
	^ super defaultTreeMorph
		dragEnabled: true;
		nodeStringGetter: #name