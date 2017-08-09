specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #redSlider;
				add: #greenSlider;
				add: #blueSlider ];
		yourself