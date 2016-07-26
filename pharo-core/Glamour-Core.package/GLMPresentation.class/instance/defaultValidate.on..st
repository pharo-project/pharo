validation
defaultValidate: anObject on: aPort 
	"by default any object is allowed in any port"
	"override this method in subclasses to specify stronger constraints"
	^ true