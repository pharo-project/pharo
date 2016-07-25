as yet unclassified
marriedMorph
	^ ( self theme  
			newCheckboxIn: World
			for: self 
			getSelected: #married
			setSelected: #married:
			getEnabled: nil
			label: '' 
			help: nil) color: Color transparent