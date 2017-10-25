examples
exampleWithNoSpecifiedNodes
	"self exampleWithNoSpecifiedNodes"

	TreePresenter new
		 roots: (1 to: 5);
		 childrenBlock: [ :item | 1+item to: 5+item ];
		 openWithSpec