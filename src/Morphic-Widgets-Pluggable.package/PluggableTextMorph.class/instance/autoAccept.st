accessing
autoAccept
	"Answer whether the editor accepts its contents on each change."

	^autoAccept ifNil: [false]