private
releaseParagraph
	paragraph
		ifNotNil: [ 
			self withoutAnyDecorator.
			paragraph unplug.
			paragraph := nil ].
