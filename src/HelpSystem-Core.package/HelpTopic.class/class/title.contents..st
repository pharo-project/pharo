instance creation
title: aTitle contents: aText
	"Create a new instance with given title and content"
	
	^(self new)
		title: aTitle;
		contents: aText;
		yourself.
		