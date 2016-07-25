accessing
groupsManager
	^ groups ifNil: [ groups := self buildGroupManager ]