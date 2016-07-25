menu
menuCommandOn: aBuilder 
	<worldMenu> 
	(aBuilder group: #SystemChanges)
		parent: #Tools;  
		order: 0.51;
		with: [
			(aBuilder item: #'Change Sorter')
				action:[self open]; 
				icon: self taskbarIcon].			
	aBuilder withSeparatorAfter.		