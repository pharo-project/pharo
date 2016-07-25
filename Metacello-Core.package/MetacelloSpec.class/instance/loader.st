accessing
loader

	loader == nil 
		ifTrue: [ | prjctLoader |
			(prjctLoader := self project loader) == nil
				ifTrue: [ self loader: (self project loaderClass on: self) ]
				ifFalse: [ self loader: prjctLoader ]].
	^loader