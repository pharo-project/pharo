action
apply

	(target isNil or: [ setSelector isNil ]) ifTrue: [ ^ self ].			
	self selectedFont ifNotNil: [ :font | target perform: setSelector with: font ].