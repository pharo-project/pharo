widget API
getTabs
	^ [ (self model tabs collect: [ :each | self buildTab: each ]) collect: #asWidget ]