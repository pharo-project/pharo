*Iceberg-Changes
diffToParent
	^ IceFullDiff 
		from: self 
		to: (self parents 
			ifNotEmpty: #first
			ifEmpty: [ nil ])