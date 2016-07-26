specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				newRow: [ :row | 
							row
								add: #packageLabel;
								add: #packageChosenLabel ]
					height: 30;
				newRow: [ :row | 
							row
								add: #textPackageField;
								add: #buttonOpen ]
					height: 30;
				newRow: [ :row | 
							row
								add: #packageList;
								add: #packageChosenList ];
				newRow: [ :row | 
							row
								add: #buttonAddAll;
								add: #buttonAddPackage;
								add: #buttonRemoveAll;
								add: #buttonRemovePackage ]
					height: 30 ] yourself