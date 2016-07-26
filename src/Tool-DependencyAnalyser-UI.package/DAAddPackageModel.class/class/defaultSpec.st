specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				add: #packageList;
				add: #buttonAddPackage height: 30
				] yourself