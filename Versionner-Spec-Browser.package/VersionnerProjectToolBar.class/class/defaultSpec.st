specs
defaultSpec
	<spec>

	"Define the layout of the Versionner ToolBar"
	^ SpecLayout composed
		newRow: [ :r | r
					add: #updateButton;
					add: #releaseButton;
					add: #commitButton ];
		yourself.