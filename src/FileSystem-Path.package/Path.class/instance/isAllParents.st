private
isAllParents
	1 to: self size do: [:i | (self at: i) = '..' ifFalse: [^ false]].
	^ true