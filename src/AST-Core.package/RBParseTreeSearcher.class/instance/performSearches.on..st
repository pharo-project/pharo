private
performSearches: aSearchCollection on: aNode 
	| value |
	aSearchCollection do: [ :aSearchRule|
		value := aSearchRule performOn: aNode.
		value notNil 
			ifTrue: 
				[self foundMatch.
				^value]].
	^nil