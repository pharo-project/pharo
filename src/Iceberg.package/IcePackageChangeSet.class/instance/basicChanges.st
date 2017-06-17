initialization
basicChanges
	| mine theirs |

	self package isLoaded ifFalse: [ ^ #() ].
	(self isAnalyseFullChanges not 
		and: [ self package isModified not and: [ self package isExported ] ]) 
		ifTrue: [ ^ #() ].
	
	mine := self parent mySnapshot: self package.
	theirs := (self package isExported 
		ifTrue: [ self parent theirSnapshot: self package ]
		ifFalse: [ MCSnapshot empty ]).
	^ (mine notNil and: [ theirs notNil ]) 
		ifTrue: [ (MCPatch fromBase: theirs target: mine) operations ]
		ifFalse: [ #() ]