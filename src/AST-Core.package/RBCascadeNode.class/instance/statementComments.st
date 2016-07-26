accessing
statementComments
	| statementComments |
	statementComments := OrderedCollection withAll: self comments.
	statementComments addAll: messages first receiver statementComments.
	messages do: 
			[:each | 
			each arguments 
				do: [:arg | statementComments addAll: arg statementComments]].
	^statementComments asSortedCollection: [:a :b | a start < b start]