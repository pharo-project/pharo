accessing
itemNamed: aName
	"Return the MenuRegistration named aName or nil if none is found"
	| result |
	self name = aName ifTrue: [^ self].
	result :=  self itemList ifNotNil: [:l | l detect: [:item | (item itemNamed: aName) notNil] ifNone: []].
	^ result
		ifNil: [ nil ]
		ifNotNil: [ result itemNamed: aName ]