specs
defaultSpec
	<spec>
	^ SpecLayout composed
		newColumn:[ :col | 
			col  
				newRow: #listPanel;
				newRow: #selectButton height: self buttonHeight ];
		yourself