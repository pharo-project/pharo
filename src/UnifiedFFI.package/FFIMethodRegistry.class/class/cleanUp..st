cleanup
cleanUp: aggressive
	"Only delete change sets when being aggressive"
	self uniqueInstance reset.
	uniqueInstance := nil
	