accessing
spec
	<spec: #default>

	^ SpecLayout composed
			newRow: [ :row | 
				row 
					add: #dateLabel width: 60; 
					"This is to address a not yet fixed Spec issue"
					newRow: [:ugly | 
						ugly
							add: #dateModel;  
							add: #chooseDateButton width: 50 ]] height: 25
			yourself.