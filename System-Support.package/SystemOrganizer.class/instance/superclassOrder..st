queries
superclassOrder: category
	"Answer an OrderedCollection containing references to the classes in the 
	category whose name is the argument, category (a string). The classes 
	are ordered with superclasses first so they can be filed in."

	| behaviors classes |
	behaviors := (self listAtCategoryNamed: category asSymbol) collect: [ :title | self environment at: title ].
	classes := behaviors select: [ :each | each isBehavior ].
	^ Class superclassOrder: classes 