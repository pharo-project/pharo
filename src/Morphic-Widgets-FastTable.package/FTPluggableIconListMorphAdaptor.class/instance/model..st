accessing
model: anObject 
	| oldModel |
	oldModel := self dataSource model. 
	oldModel ifNotNil: [ oldModel removeDependent: self ].
	anObject ifNotNil: [ anObject addDependent: self ].
	self dataSource model: anObject