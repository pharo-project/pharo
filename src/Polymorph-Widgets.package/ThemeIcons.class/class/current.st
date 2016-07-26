instance creation
current 
	"WARNING: Direct access to this method is ill-adviced, use Smalltalk ui icons instead."
	^ Current ifNil: [ Current := self loadDefault ]