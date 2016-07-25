browse-nautilus
browseMethodInNautilus: aMethod
	| classToBrowse nautilus compiledMethod nautilusUI |
	nautilus := self rootNode packageTreeNautilus.
	nautilusUI := self rootNode packageTreeNautilusUI.
	classToBrowse := aMethod methodClass.
	compiledMethod := classToBrowse>>aMethod selector.
	self setSelectionOnMethod: compiledMethod class: classToBrowse with: nautilus.
	nautilusUI updateCategoryAndMethodList.
	nautilusUI sourceTextModel announce: NautilusMethodSelected.	
	
	