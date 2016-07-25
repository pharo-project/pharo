api
buildChildren

	contents := self children value collect: [ :each || node | 
		node := each isTreeNodeModel
			ifTrue: [ each ]
			ifFalse: [  (self childNodeClassFor: each) new
				content: each  ].
		node 
			parentNode: self;
			yourself ].
		
	^ contents