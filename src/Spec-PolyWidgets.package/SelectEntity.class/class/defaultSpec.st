specs
defaultSpec
	<spec: #default>
		
	^SpecLayout composed
		newColumn: [ :col |
				col newRow: [:row |
					row add: #entityText;
						add: #selectEntity width: ComposablePresenter toolbarHeight]
					height: ComposablePresenter toolbarHeight];
		yourself
