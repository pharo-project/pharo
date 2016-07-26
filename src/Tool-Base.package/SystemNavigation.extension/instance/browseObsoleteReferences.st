*Tool-Base
browseObsoleteReferences  
	"self new browseObsoleteReferences"

	| references |
	references := OrderedCollection new.
	(LookupKey allSubInstances select:
		[:x | (x value isBehavior and: ['AnOb*' match: x value name]) or:
		['AnOb*' match: x value class name]]) 
		do: [:x | references addAll: (self allReferencesTo: x)].

	^ self  
		browseMessageList: references 
		name: 'References to Obsolete Classes'