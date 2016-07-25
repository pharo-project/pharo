*Tool-Base
browseMethodsWithSourceString: aString matchCase: caseSensitive
	"SystemNavigation new browseMethodsWithSourceString: 'SourceString'"
	"Launch a browser on all methods whose source code, inluding string literals and comments, contains aString as a substring."
	| suffix |

	suffix := caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (case-insensitive)'].
	^ self
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive)
		name: 'Methods containing ' , aString printString , suffix
		autoSelect: aString