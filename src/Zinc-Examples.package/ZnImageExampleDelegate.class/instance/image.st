accessing
image
	"Return the current image entity.
	Use a default when not yet set."
	
	^ image ifNil: [ image := self downloadPharoLogo ]