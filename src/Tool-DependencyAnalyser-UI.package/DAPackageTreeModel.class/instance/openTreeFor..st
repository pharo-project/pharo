actions
openTreeFor: namesOfSelected
	| treeToOpen relationGraph |
	treeToOpen := TreePresenter new.
	relationGraph := self tree owner relationGraph.
	^ treeToOpen
		roots: namesOfSelected;
		whenBuiltDo: [ 
					treeToOpen
						rootNodeHolder: [ :item | 
							DAClassToDependencyNode new
								content: item;
								package: (relationGraph pdPackageFrom: item) ] ];
		title: 'Class dependencies';
		extent: 700 @ 500;
		openWithSpec