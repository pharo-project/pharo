visitor
visitMonticelloVersionsLoad: aChange
	"Reload *from cache*."
	(aChange versionNames 
		inject: Gofer new
		into: [ :gofer :versionName | gofer version: versionName ]) load.