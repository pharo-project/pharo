examples
exampleOfAutoRefreshOnExpand
	"Each time a node is expanded, the computation of the children is made again, instead of being cached as it is usually"

	TreePresenter new
		childrenBlock: [ :number | 
			self logCr: 'Compute children of ', number asString.
			1 to: number ];
		hasChildrenBlock: [ :number | true ];
		roots: (1 to: 10);
		autoRefreshOnExpand: true;
		openWithSpec