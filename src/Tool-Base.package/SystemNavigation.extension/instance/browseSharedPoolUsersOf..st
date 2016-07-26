*Tool-Base
browseSharedPoolUsersOf: aClass

	| pattern list | 
	self okToChange ifFalse: [^ self ].
	list := self allClassesUsingSharedPool: aClass name.
	list isEmpty
		ifTrue: [ ^ self ]
		ifFalse: [pattern := UIManager default 
				enterOrRequestFrom: list 
				lines: #() 
				title: 'Class name or fragment?'].

	pattern ifNil: [^ self ].
	pattern browse