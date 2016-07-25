private
createWindowHandleFor: osWindow
	^ (preferableDriver
		ifNil: [ preferableDriver := OSWindowDriver current ])
		createWindowWithAttributes: self
		osWindow: osWindow