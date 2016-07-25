specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed
		newRow: [ :top |
			top
				add: #changes;
				addVSplitter;
				add: #commit
		] bottom: 0.5;
		addHSplitter;
		newRow: [ :bottom | bottom add: #diff ] top: 0.5;
		yourself.
