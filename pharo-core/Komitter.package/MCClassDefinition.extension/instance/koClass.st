*Komitter-UI
koClass
	self className ifNil: [ ^ nil ] ifNotNil: [ :cname | ^ KomitClass trackedClass: cname ]