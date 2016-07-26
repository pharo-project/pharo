accessing
methodsDo: aBlock
	
	self classesDo: [ :aClass |
		
		self selectorsForClass: aClass do: [ :selector |
			aBlock value: (aClass >> selector) ].
		self selectorsForClass: aClass class do: [ :selector |
			aBlock value: (aClass class >> selector) ] ]