specs
defaultSpec
	<spec>
	^ SpecLayout composed
		newColumn: [ :c | c newRow: [ :r | 
							r
								add: #filterField;
								add: #clearFilterButton width: 25 ]
						height: self toolbarHeight.
					c newRow: [ :r | r add: #kmTable ] ];
		yourself