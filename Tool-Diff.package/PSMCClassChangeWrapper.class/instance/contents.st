accessing
contents
	"Answer the contents of the change."

	^contents ifNil: [contents := self gatherContents]