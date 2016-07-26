instance creation
newForMethod: aMethod
	"This is the only method for creating new contexts, other than primitive cloning.
	Any other attempts, such as inherited methods like shallowCopy, should be
	avoided or must at least be rewritten to determine the proper size from the
	method being activated.  This is because asking a context its size (even basicSize!)
	will not return the real object size but only the number of fields currently
	accessible, as determined by stackp."

	^ super basicNew: aMethod frameSize