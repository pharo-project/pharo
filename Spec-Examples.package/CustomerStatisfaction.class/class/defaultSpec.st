specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | row add: #screen ] origin: 0@0 corner: 1@0.2 ;
		newRow: [ :row | 
			row
				add: #buttonHappy;
				add: #buttonNeutral;
				add: #buttonBad ]
			origin: 0@0.2 corner: 1@1;
		yourself