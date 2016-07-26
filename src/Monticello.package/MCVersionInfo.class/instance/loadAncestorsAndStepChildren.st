private
loadAncestorsAndStepChildren
	"This code scans all the repositories to find the exact package version and reload the relevant info."

	"Transient failures, such as the lack of network connectivity, will return nothing and no errors from this code."

	"Unused in MCVersionInfo, used only in MCLazyVersionInfo."

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"

	(MCRepositoryGroup default
		in: [ :r | r versionWithInfo: self ifNone: [  ] ])
		ifNotNil:
			[ :aVersion | 
			self setAncestors: aVersion info ancestors.
			self setStepChildren: aVersion info stepChildren ]