spec
defaultSpec
	<spec>

	"Define the layout of the Versionner configuration list toolBar"
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #newButton;
				add: #loadButton;
				add: #openButton ];
	yourself.