as yet unclassified
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :c | 
			c 
				add: #text ];
		yourself.
	 