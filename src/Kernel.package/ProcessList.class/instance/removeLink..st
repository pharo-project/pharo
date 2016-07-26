removing
removeLink: aLink
	^self removeLink: aLink ifAbsent: [self error: 'no such method!']