file list menu
fileSelectedMenu: aMenu
	| n1 n2 n3 services |
	services := OrderedCollection withAll: (self itemsForFile: reference).
	n1 := services size.
	services addAll: self itemsForAnyFile.
	n2 := services size.
	services addAll: self itemsForNoFile.
	n3 := services size.
	services := services collect: [ :svc | svc copy.].
	services do: [ :svc | svc addDependent: self; doNotUseLineAfter ].
	^ aMenu 
		addServices: services 
		for: self
		extraLines: (Array with: n1 with: n2 with: n3)
