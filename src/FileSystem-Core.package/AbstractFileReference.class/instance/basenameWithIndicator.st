accessing
basenameWithIndicator
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is 'gloops.taz', whereras /foo basenameWithIndicator is 'foo/'" 
	^ self basename, self indicator   