accessing
calculateChildren
	^ self getChildren
		collect: [ :elem | 
			(self class data: elem from: dataSource)
				depth: self depth + 1;
				yourself ]