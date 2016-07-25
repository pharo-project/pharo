accessing
defaultSortBlock
	"Cache the default sort block here.
	Since the sortBlock is used to compare instances, a single instance for the default block helps us.."
	^ sortBlock ifNil: [ 
		sortBlock := [ :a :b | a <= b]]