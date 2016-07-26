spec
inspectorSpec
	<spec>
	
	^ SpecLayout composed
		newRow: #container bottom: 0.3;
		addHSplitter;
		newRow: #text top: 0.7;
		yourself