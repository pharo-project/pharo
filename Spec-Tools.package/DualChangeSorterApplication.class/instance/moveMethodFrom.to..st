menu - method
moveMethodFrom: src to: dest

	self copyMethodFrom: src to: dest.
	src forgetMessage.
	src updateClassesListAndMessagesList.