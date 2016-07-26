private
paragraph
	"Paragraph instantiation is lazy -- create it only when needed"

	paragraph
		ifNil: [
			paragraph := self newParagraph.
			self compose.
			self selectAll].
	^ paragraph 