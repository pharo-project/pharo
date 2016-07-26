open/close
initialExtent
	"Answer the default extent for the receiver."

	|rl paneExt ext|
	rl := self getRawLabel.
	paneExt := self mainPanel
		ifNil: [0@0]
		ifNotNil: [:pane | pane minExtent].
	ext := paneExt + (2@ self labelHeight) + (2 * self class borderWidth)
		max: rl extent + 20.
	self isResizeable ifTrue: [
		self title: self title "adjust minimumExtent".
		self minimumExtent: (ext x max: self minimumExtent x)@(ext y max: self minimumExtent y)].
	^ext