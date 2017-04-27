accessing
directory

	^ directory ifNil: [ 
		directory := self monitor sessionStore baseLocator ]