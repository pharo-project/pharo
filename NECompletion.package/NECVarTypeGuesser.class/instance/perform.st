public
perform
	| infos infosOfMethod methodRefs |
	variableName isEmpty
		ifTrue: [^ nil].
	methodRefs := self methodRefs.
	methodRefs
		ifNil: [^ nil]. 
	methodRefs isBehavior
		ifTrue: [^ methodRefs].
	contextCount := 0.
	infos := SortedCollection
				sortBlock: [:a :b | a priority <= b priority].
	methodRefs
		do: [:each | 
			self prepare: each.
			infosOfMethod := self typeOfVarIn: each compiledMethod.
			infos addAll: infosOfMethod].
	infos notEmpty
		ifTrue: [^ infos first type].
	^ nil