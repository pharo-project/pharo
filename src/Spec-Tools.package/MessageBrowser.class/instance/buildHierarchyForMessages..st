testing
buildHierarchyForMessages: messages
	| result classes |
	
	result := Dictionary new.
	self topologicSort 
		ifFalse: [ 
			messages do: [:m | result at: m put: {} ].
			^ result ].
	
	classes := (messages collect: #methodClass) asSet.
	messages do: [:message || level class |
		class := message methodClass.
		level := OrderedCollection new.
		class allSuperclassesDo: [:superClass |
			(classes includes: superClass)
				ifTrue: [ level addFirst: superClass ]].
		level addLast: class.
		level addLast: message selector.
		result at: message put: level ].
	^ result