specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				newRow: [ :row | 
							row
								add: #textPackageField;
								add: #buttonBrowseCycles;
								add: #buttonRefresh;
								add: #buttonDefault
								 ]
					height: 30;
				newRow: [ :row | 
							row
								add: #packageLabel ]
					height: 20;
				add: #tree;
				newRow: [ :row | 
							row
								add: #buttonAddPackage;
								add: #buttonRemovePackage;
								"add: #buttonGenerateGraphViz;"
								"add: #buttonMakeDiff;"
								add: #buttonReverseAnalysis
								 ]
					height: 30 ]	
					  yourself