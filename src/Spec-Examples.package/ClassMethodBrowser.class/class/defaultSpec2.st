specs
defaultSpec2
<spec: #default>

	^ SpecLayout composed
		newRow: [:r | 
			r 
				newColumn: #listModel right: 0.5;
				addVSplitter;
				newColumn: #(methodModel listModel) left: 0.5 ] bottom: 0.5;
		addHSplitter;
		newRow: #(methodModel textModel) top: 0.5;
		yourself