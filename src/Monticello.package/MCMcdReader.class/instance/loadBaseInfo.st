loading
loadBaseInfo
	^ baseInfo := self extractInfoFrom: (self parseMember: 'base')