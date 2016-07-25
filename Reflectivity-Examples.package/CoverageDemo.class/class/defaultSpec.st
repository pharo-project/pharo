specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [:col|
			col add: #codePanelMorph.
			col newRow: [ :row|
				row add: #installMetalinkButton.
				row add: #runCodeButton.	
			] height:  (self toolbarHeight)
		]