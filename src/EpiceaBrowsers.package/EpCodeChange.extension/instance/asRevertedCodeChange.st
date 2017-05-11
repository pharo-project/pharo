*EpiceaBrowsers
asRevertedCodeChange
	"Answer a code change that represents my inverse operation. For example, a class addition will answer a class removal."

	^ self accept: EpInverseVisitor new
