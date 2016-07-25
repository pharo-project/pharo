accessing
buildMenuSpec: aMenu 
	"recursiveley build the menu spec aMenu passed as argument"
	self itemList
		ifNotNil: [:l | 
			| m |
			m := isGroup 
				ifFalse: [PluggableMenuSpec withModel: nil]
				ifTrue: [aMenu].
			(l reject: [:i | i precondition value not]) do: [:i | i buildMenuSpec: m].
			isGroup ifFalse: [self spec subMenu: m]].
	isGroup
		ifTrue: [self spec separator ifTrue: [self itemList ifNotNil: [self itemList last spec separator: true]]] 
		ifFalse: [aMenu items add: self spec]