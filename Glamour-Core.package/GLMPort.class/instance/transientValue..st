accessing
transientValue: anObject 
	| previousValue |
	previousValue := self value. 
	self value: anObject.
	self silentValue: previousValue