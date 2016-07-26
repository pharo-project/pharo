private
findMethodWith: anECContext do: foundBlock ifAbsent: notfoundBlock 
	| theClass result implementors |
	theClass := anECContext model theClass.
	result := theClass
				ifNil: [implementors := self systemNavigation allImplementorsOf: contents.
					implementors size == 1
						ifTrue: [| ref | 
							ref := implementors first.
							self lookupSelector: ref selector class: ref realClass]
						ifFalse: [^ notfoundBlock value: contents]]
				ifNotNil: [self lookupSelector: contents class: theClass]. 
	^ foundBlock value: result first value: result second