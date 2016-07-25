menu - message
forgetMessage

	self okToChange ifFalse: [^ self].
	self selectedSelector ifNotNil: [:selector | 
		self model 
			forgetSelector: selector 
			inClass: self selectedClass 
			fromChangeSet: self selectedChangeSet.
		self updateClassesList.
		self setSelectedClassIndex: self selectedClassIndex ]