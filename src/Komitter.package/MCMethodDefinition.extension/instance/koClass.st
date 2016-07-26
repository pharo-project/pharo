*Komitter-UI
koClass
	^ self className 
		ifNil: [ nil ] 
		ifNotNil: [ :cname |
			self isExtensionMethod 
				ifTrue: [ KomitClass trackedClass: cname forExtension: self  protocol ]
				ifFalse: [ KomitClass trackedClass: cname ] ]