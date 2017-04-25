instance creation
title: aTitle iconName: anIconName contents: aText
	"Create a new instance with given title, icon and content"
	
	^(self new)
		title: aTitle;
		iconName: anIconName;
		contents: aText;
		yourself.
		