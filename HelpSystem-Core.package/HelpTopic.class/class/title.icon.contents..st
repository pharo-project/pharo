instance creation
title: aTitle icon: anIcon contents: aText
	"Create a new instance with given title, icon and content"
	
	^(self new)
		title: aTitle;
		icon: anIcon;
		contents: aText;
		yourself.
		