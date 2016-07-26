specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed newColumn:
		[ :col | col
			add: #message;
			newRow: [ :row | row
				add: #commitButton;
				add: #refreshButton
			] height: self toolbarHeight
		]
