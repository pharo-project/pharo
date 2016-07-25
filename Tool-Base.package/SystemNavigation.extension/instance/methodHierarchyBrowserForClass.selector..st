*Tool-Base
methodHierarchyBrowserForClass: aClass selector: sel
	"Create and schedule a message set browser on all implementors of the 
	currently selected message selector. Do nothing if no message is selected."
	
	| list |
	aClass ifNil: [^ self].
	aClass isTrait ifTrue: [^ self].
	sel ifNil: [^ self].
	list := OrderedCollection new.

	aClass allSuperclasses reverseDo: [:cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (self createMethodNamed: sel realParent: cl)]].
	aClass allSubclassesDo: [:cl | 
		(cl includesSelector: sel) ifTrue: [
			list addLast: ( self createMethodNamed: sel realParent: cl)]].

	list addLast: (self createMethodNamed: sel realParent: aClass).
	^ self browseMessageList: list name: 'Inheritance of ' , sel

