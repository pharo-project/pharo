spec
inspectorSpec
	<spec>
	
	^ SpecLayout composed
		newRow: #tree bottom: 0.3;
		addHSplitter;
		newRow: #text top: 0.7;
		yourself