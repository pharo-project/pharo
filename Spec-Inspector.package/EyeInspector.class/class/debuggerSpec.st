spec
debuggerSpec
	<spec:#default>
	
	^ SpecLayout composed
		add: #list origin: 0@0 corner: 0.33@1;
		addVSplitter;
		add: #description origin: 0.33@0 corner: 1@1;
		yourself