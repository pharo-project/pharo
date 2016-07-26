*NautilusRefactoring
whatToDisplayIn: aBrowser

	^ self changes flatCollect: [:each | each whatToDisplayIn: aBrowser ]