printing
clipText
	"Copy the text in the receiver or in its submorphs to the clipboard"
	| content |
	"My own text"
	content := self userString.
	"Or in my submorphs"
	content ifNil: [
		| list |
		list := self allStringsAfter: nil.
		list notEmpty ifTrue: [
			content := String streamContents: [:stream |
				list do: [:each | stream nextPutAll: each; cr]]]].
	"Did we find something?"
	content
		ifNil: [self flash "provide feedback"]
		ifNotNil: [Clipboard clipboardText: content].