menu - class
forgetClass
	"Remove all mention of this class from the changeSet"

	self okToChange ifFalse: [^ self].
	self selectedClass ifNotNil: [:class |
		self model removeClass: class from: self selectedChangeSet.
		self setSelectedChangeSet: self selectedChangeSet ].
