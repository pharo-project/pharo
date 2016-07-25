*Tool-Base
browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Launch a class-list list browser on all classes whose names containg aString as a substring."
	"SystemNavigation default browseClassesWithNamesContaining: 'Morph' caseSensitive: true "
	
	|  classes |
	classes :=  self environment allClasses select: [:class | (class name includesSubstring: aString caseSensitive: caseSensitive)].				
	^Smalltalk tools browser openInEnvironment: (RBBrowserEnvironment new forClasses: classes).
									