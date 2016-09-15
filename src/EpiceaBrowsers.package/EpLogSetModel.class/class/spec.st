accessing
spec
	<spec: #default>

	^ SpecLayout composed
		newRow: [ :row |
			row 
				add: #logNodesModel origin: 0@0 corner: 0.4@1;
				addSplitter;
				add: #logModel origin: 0.4@0 corner: 1@1 ]