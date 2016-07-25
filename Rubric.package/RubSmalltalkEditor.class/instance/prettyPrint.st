menu messages
prettyPrint
	"Reformat the contents of the receiver's view (a Browser)."

	| selectedClass newText |
	self model selectedMessageName ifNil: [^ textArea flash].
	selectedClass := self modelCurrentSelectedClass.
	newText := selectedClass compilerClass new
		format: self text
		in: selectedClass
		notifying: self.
	newText ifNotNil:
		[ self selectInvisiblyFrom: 1 to: self paragraph text size.
		self replaceSelectionWith: (newText asText makeSelectorBoldIn: selectedClass).
		self selectAt: 1]