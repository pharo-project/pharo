converting
asHTMLString
	"substitute the < & > into HTML compliant elements"
	"'<a>' asHTMLString"
	^ self class new: self size streamContents: [ :s| 
		self do: [:c | s nextPutAll: c asHTMLString ]]
