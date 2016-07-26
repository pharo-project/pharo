instance creation
openStrongOn: anObject
	"only show strong references"
	^(self new 
		onlyStrong;
		inspect: anObject) openWithSpec