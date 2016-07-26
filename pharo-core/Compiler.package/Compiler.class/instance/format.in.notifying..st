public - old
format: textOrStream in: aClass notifying: aRequestor
	"Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely.  If aBoolean is true, then decorate the resulting text with color and hypertext actions"
	| aNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	aNode := self format: sourceStream noPattern: false ifFail: [^ nil].
	^ aNode formattedCode.