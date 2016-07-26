accessing
value 
	self flag: 'checking for #entity is rather not elegant, but it is a solution to not affect the other custom ports that will probably be desired to be populated in other ways.'.
	^ self name == #entity 
		ifTrue: [ self presentation transformation glamourValue: self port value asGlamorousMultiValue ] 
		ifFalse: [ self port value ]