private
items
	^ items ifNil: [ 
		items := self defaultItems.
		self itemDo: [ :each | 
			items add: each ] ]