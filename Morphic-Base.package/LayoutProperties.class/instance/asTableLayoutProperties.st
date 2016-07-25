converting
asTableLayoutProperties
	^(TableLayoutProperties new)
		hResizing: self hResizing;
		vResizing: self vResizing;
		disableTableLayout: self disableTableLayout;
		yourself