testing
isResumable
	"Determine whether an exception is resumable. By default not but can be resumable using accessors at exception raising time."

	^ resumable ifNil: [ super isResumable ]