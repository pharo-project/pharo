*Tool-Base
browseObsoleteMethodReferences
	"Open a browser on all referenced behaviors that are obsolete"

	"SystemNavigation new browseObsoleteMethodReferences"

	| list |
	list := self obsoleteMethodReferences.
	^ self 
		browseMessageList: list
		name: 'Method referencing obsoletes'
		autoSelect: nil