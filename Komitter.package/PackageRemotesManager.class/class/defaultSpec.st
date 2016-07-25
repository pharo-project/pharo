specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [ :row |
			row
				add: #packages;
				addSplitter;
				add: #remotes
		];
		yourself