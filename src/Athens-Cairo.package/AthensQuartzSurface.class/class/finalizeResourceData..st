as yet unclassified
finalizeResourceData: data
	| handle contextHandle |
	handle := data first.
	"	id := data second.  - id is not used "
	contextHandle := data third.
	
	self destroyContextHandle: contextHandle value. 
	self destroySurfaceHandle: handle value