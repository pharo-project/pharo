private
privateOwner: newOwner
	"Nil the container when text gets extracted"
	super privateOwner: newOwner.
	container ifNotNil: [
		newOwner ifNotNil: [
			newOwner isWorldOrHandMorph ifTrue: [self setContainer: nil]]]