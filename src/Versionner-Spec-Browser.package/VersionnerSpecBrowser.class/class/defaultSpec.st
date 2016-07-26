spec
defaultSpec
	"Define the layout of the Versionner Browser"
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #toolbar height: self toolbarHeight;
				add: #configurationList ];
	yourself.
 