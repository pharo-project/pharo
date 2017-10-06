specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #preview;
				add: #sliders ];
		yourself