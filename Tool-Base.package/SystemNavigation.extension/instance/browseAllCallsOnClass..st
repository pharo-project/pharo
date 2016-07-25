*Tool-Base
browseAllCallsOnClass: aClass
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, SystemNavigation new browseAllCallsOnClass: Object."

	^ self
		browseMessageList: ((aClass allCallsOnIn: self) asSortedCollection)
		name: 'Users of class ' , aClass theNonMetaClass name
		autoSelect: aClass theNonMetaClass name