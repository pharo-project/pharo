*Ring-Core-Kernel
createCommentDefinition
	"Retrieves a RGCommentDefinition object based on the data of the receiver"

	^ (RGCommentDefinition class: self commentClass)
		content: self string;
		stamp: stamp;
		yourself 