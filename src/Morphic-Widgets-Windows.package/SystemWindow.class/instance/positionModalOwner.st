activation
positionModalOwner

	self modalOwner ifNotNil: [ :modalOwner |
		(modalOwner isKindOf: SystemWindow) ifTrue: [ 
			modalOwner bringBehind: self.
		]
   ]
