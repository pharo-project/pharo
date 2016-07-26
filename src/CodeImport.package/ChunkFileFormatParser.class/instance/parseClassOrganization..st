parsing
parseClassOrganization: classOrganizationPreamble
	"The comment preable is an array with the following structure:
	If instance side method:
	#(#CodeImportTestCaseTestClass #reorganize)
	
	if class side:
	
	#(#CodeImportTestCaseTestClass #class #reorganize)
	"

	| behaviorName isMeta |
	behaviorName := classOrganizationPreamble first.
	isMeta := classOrganizationPreamble second ~= self reorganizeSelector.
	self
		addDeclaration:
			(self classOrganisationChunkClass
				contents: readStream next
				behaviorName: behaviorName
				isMeta: isMeta)