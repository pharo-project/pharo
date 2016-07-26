pointing to
pointsTo: anObject
	"Answers true if I hold a reference to anObject, or false otherwise
	an object points to a class via the header either directly or indirectly 
	via the compact classes array"

	^ (self instVarsInclude: anObject) or: [ ^self class == anObject]