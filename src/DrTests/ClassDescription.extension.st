Extension { #name : 'ClassDescription' }

{ #category : '*DrTests' }
ClassDescription >> drTestsBrowse [
	"May be using double dispatch would be better
	so that the application of DrTest can open tools 
	without relying on this global."

	(Smalltalk tools toolNamed: #browser) openOnClass: self
]
