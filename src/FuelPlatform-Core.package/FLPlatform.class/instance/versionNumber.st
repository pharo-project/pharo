accessing
versionNumber
	^ ('\d+' asRegex matchesIn: self className) first asInteger