selecting
strongSelection: ann
	self announcer
		announce:
			(GLMTreeMorphStrongSelectionChanged new
				strongSelectionValue: (self elementAt: ann selectedRowIndex) data;
				yourself)