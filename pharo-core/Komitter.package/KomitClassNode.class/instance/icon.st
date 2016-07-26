accessing
icon
	
	self content added ifTrue: [ ^ self class addedClassIcon ].
	self content modified ifTrue: [ ^ self class modifiedClassIcon ].
	self content removed ifTrue: [ ^ self class deletedClassIcon ].
	
	^ self class defaultClassIcon