building
buildMorphWindowForTreeModel
	| specWindow |
	packageAnalyzerTreeModel
		open;
		delete.
	
	specWindow := self specWindowFrom: packageAnalyzerTreeModel.
	^ specWindow