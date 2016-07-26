menu messages
prettyPrint
	"Reformat the contents of the receiver's view (a Browser)."

	| selectedClass newText |
	model selectedMessageName ifNil: [^ morph flash].
	selectedClass := self modelCurrentSelectedClass.
	newText := selectedClass compiler
		source: self text;
		class: selectedClass;
		requestor: self;
		format.
	newText ifNotNil:
		[ self selectInvisiblyFrom: 1 to: self paragraph text size.
		self replaceSelectionWith: newText.
		self selectAt: 1]