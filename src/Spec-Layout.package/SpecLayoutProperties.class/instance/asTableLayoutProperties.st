converting
asTableLayoutProperties
	^(SpecTableLayoutProperties new)
		hResizing: self hResizing;
		vResizing: self vResizing;
		disableTableLayout: self disableTableLayout;
		yourself