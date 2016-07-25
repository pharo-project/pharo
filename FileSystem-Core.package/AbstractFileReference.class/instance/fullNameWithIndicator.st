accessing
fullNameWithIndicator
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is '/foo/gloops.taz', whereras /foo basenameWithIndicator is '/foo/'" 
	^ self fullName, self indicator   