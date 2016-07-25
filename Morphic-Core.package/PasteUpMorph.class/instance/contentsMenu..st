menu & halo
contentsMenu: aMenu
	"Build the menu used from PopUpContentsMenu:"
	(self submorphs asSortedCollection: [:w1 :w2 | w1 class name caseInsensitiveLessOrEqual: w2 class name]) do:
		[:w | aMenu add: w class name target: w selector: #comeToFront ].
	^ aMenu