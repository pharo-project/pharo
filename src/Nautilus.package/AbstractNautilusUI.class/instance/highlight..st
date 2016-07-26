source code area
highlight: autoSelectString
	| first |

	first := self sourceTextModel getString findString: autoSelectString.
	contentSelection := first to: first+autoSelectString size-1.
	self updateSourceCodeAreaWith: self sourceCode