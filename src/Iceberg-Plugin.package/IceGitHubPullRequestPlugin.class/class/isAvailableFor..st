testing
isAvailableFor: aRepository
	"validate if this is a github repository"
	^ aRepository origin host = 'github.com'