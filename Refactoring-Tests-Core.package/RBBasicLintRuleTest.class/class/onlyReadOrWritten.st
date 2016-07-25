unnecessary code
onlyReadOrWritten
	| detector |
	detector := self new.
	detector name: 'Instance variables not read AND written'.
	detector result: nil references.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					| reads writes |
					reads := false.
					writes := false.
					allSubclasses detect: 
							[:class | 
							reads ifFalse: [reads := (class whichSelectorsRead: each) isEmpty not].
							writes ifFalse: [writes := (class whichSelectorsWrite: each) isEmpty not].
							reads & writes]
						ifNone: [result addInstVar: each for: context selectedClass]]].
	^detector