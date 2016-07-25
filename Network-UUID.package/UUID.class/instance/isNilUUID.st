testing
isNilUUID
	1 to: self size do: [:i | (self at: i) ~= 0 ifTrue: [^false]].
	^true.