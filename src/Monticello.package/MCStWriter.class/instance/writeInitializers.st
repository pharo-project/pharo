accessing
writeInitializers

	"answer if class initializers should be written"

	^ writeInitializers ifNil: [ writeInitializers := true ]