accessing
resolved
	"Answer the resolved references of the receiver."

	^ self references collect: [ :each | each resolveWith: self ]