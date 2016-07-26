updating
updateHeaderRow
	"Recalculates the header row if they are defined. 
	 Please, note that If one of the headers is nil, I assume all are nil and I return. 
	 This is probably not the best approach, but like that I enforce people defines at least 
	 a default if they want headers."
	| columnHeaders columnWidths previousDisplayColumn |

	self canRefreshValues ifFalse: [ ^ self ].

	headerRow := nil.	
	columnHeaders := Array new: self owner numberOfColumns.
	columnWidths := self calculateColumnWidths.
	
	self owner columns withIndexDo: [ :each :index | | headerCell columnWidth|
		columnWidth := columnWidths at: index.
		headerCell :=  self owner dataSource headerColumn: each. 
		headerCell ifNil: [ ^ self ]. 
		headerCell 
			color: self owner headerColor;
			width: columnWidth.
		columnHeaders at: index put: headerCell.
		previousDisplayColumn := FTDisplayColumn column: each width: columnWidth ].
	 
	headerRow := (FTTableRowMorph table: self owner)
		privateOwner: self;
		addAllMorphs: columnHeaders;
		yourself