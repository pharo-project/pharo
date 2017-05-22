accessing
tagsForRemote: aRemote
	| tags |
	
	tags := OrderedCollection new.
	(self isPushRemote: aRemote) ifTrue: [ tags add: self class tagDefaultPush ]. 
	(self isPullRemote: aRemote) ifTrue: [ tags add: self class tagDefaultPull ]. 
	
	^ tags asArray