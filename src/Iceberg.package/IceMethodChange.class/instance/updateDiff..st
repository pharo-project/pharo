*Iceberg-UI
updateDiff: aDiffModel 
	aDiffModel
		contextClass: self operation targetClass;
		leftText: self operation toSource;
		rightText: self operation fromSource.