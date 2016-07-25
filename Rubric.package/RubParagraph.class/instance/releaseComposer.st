private
releaseComposer
	composer
		ifNotNil: [ :c | 
			c unplug.
			composer := nil ]