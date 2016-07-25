initialize - release
initialize
	self flag: #removeIt.
	"This IF will be eliminated soon...only here temporary"
	(SystemVersion current highestUpdate >= 50558)
		ifTrue: [ 
			SessionManager default registerToolClassNamed: self name
			 ]
		ifFalse: [ 
			Smalltalk addToStartUpList: self.
 		]
	