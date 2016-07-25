specs
inspectorSpec
	<spec>
	
	^ SpecLayout composed
		newColumn: [ :c | 
			c 
				add: #toolbar height: self toolbarHeight;
				add: #inspector withSpec: #inspectorSpec ];
		yourself