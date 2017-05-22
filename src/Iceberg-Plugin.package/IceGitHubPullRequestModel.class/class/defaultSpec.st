specs
defaultSpec 
	<spec>

	^ SpecLayout composed
		newColumn: [ :col |
			col 
				newRow: [:row | row add: #titleLabel width: 80; add: #titlePanel ] height: 25;
				newRow: [:row | 
					row 
						newColumn: #headLabel width: 80;
						newColumn: [ :col2 | 
							col2 newRow: [ :row2 | 
								row2
									add: #headForkList;
									add: #headBranchList ] ] ] height: 25;
				newRow: [:row | 
					row 
						newColumn: #baseLabel width: 80;
						newColumn: [ :col2 |
							col2 newRow: [ :row2 |
								row2 
									add: #baseForkList;
									add: #baseBranchList ] ] ] height: 25;
				newRow: [:row | row add: #bodyLabel width: 80; add: #bodyPanel ];
				add: #addButton height: self buttonHeight  ];
		yourself