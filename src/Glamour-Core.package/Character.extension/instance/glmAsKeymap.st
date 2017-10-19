*Glamour-Core
glmAsKeymap
	^ self isUppercase 
			ifTrue: [ self meta shift ]
			ifFalse: [ self meta ]