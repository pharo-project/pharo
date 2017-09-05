accessing
commitHashShort

	"Answer the shorten hash of a Git commit from which the image was bootstrapped, e.g. '9ce829e"

	^ commitHash ifNotNil: [ commitHash first: (7 min: commitHash size) ]