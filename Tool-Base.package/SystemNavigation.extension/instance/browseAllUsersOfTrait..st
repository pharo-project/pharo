*Tool-Base
browseAllUsersOfTrait: aTrait
	"Create and openenvironment  a browser on each class or trait that imports aTrait"

	^Smalltalk tools browser openInEnvironment: (RBBrowserEnvironment new forClasses: aTrait traitUsers)
	