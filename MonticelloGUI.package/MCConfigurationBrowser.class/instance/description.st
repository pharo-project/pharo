description
description
	self selectedDependency ifNotNil: [:dep | ^ ('Package: ', dep package name, String cr,
		dep versionInfo summary) asText].
	self selectedRepository ifNotNil: [:repo | ^repo creationTemplate
		ifNotNil: [repo creationTemplate asText]
		ifNil: [repo asCreationTemplate asText addAttribute: TextColor red]].
	^ ''
