accessing
selectedFromPackageGroupView: aNode
	| selections |

	selections := aNode selectedNodes collect: [ :each |each asNautilusSelection].
	"leave if selection contains no node that isn't already selected"
	(selections allSatisfy:[:each | self isSelectedPackage:each]) ifTrue:[^ self].
	
	self clearPackageSelections.

	selections do:[: each |  self selectedPackageWithoutChangingSelection: each].
	
	self changed: #currentHistoryIndex.
