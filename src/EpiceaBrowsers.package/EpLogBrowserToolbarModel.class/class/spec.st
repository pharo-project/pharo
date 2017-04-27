specs
spec
	<spec: #default>

	^ SpecLayout composed newRow: [:row |
		row
			add: #statusLabelModel;
			add: #filtersButtonModel width: 150;
			yourself ].