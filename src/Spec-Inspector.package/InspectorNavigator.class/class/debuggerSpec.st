specs
debuggerSpec
	<spec: #default>
	
	^ SpecLayout composed
		newRow: #toolbar height: self toolbarHeight;
		add: #inspector withSpec: #debuggerSpec top: self toolbarHeight;
		yourself