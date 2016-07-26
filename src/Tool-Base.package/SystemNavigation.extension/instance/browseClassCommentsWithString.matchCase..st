*Tool-Base
browseClassCommentsWithString: aString matchCase: caseSensitive
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."

	|  suffix list |
	suffix := caseSensitive 
		ifTrue: [' (case-sensitive)']
		ifFalse: [' (case-insensitive)'].
	list := Set new.
	Cursor wait showWhile: [
		self environment allClassesDo: [:class | 
			(class organization classComment asString 
							includesSubstring: aString caseSensitive: caseSensitive) ifTrue: [
								list add: (RGCommentDefinition realClass: class)
							]
		]
	].
	^ self 
		browseMessageList: list asSortedCollection
		name: 'Class comments containing ' , aString printString , suffix
		autoSelect: aString