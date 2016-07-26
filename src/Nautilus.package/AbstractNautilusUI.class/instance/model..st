accessing
model: aNautilusModel

	model := aNautilusModel.

	aNautilusModel selectedPackage ifNotNil: [:e | self packageWidget selectPackage: e].
	aNautilusModel selectedGroup ifNotNil: [:p | self packageWidget selectGroup:p].
	aNautilusModel selectedClass ifNotNil: [:p | self classWidget selectClass: p].