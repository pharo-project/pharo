initialization
initialize	
	super initialize.
	
	self whenSelectedChanged: [ :aBoolean |
		self parentNode ifNotNil: [ :parent | parent checkChildren ].
		self ifNotSilentDo: [
			aBoolean
				ifTrue: [ self selectAllChildren ]
				ifFalse: [ self unselectAllChildren ] ] ].
		
	self whenContentChanged: [ :c || childrenNode |
		childrenNode := self getDefinitions 
			collect: [ :each | KomitDefinitionNode new
				content: each;
				yourself ].
			childrenNode addAll: (self getMethods
				collect: [ :each | KomitMethodNode new
					content: each;
					yourself ]).
		self children: [ childrenNode ] ]