*GT-InspectorExtensions-Core
gtInspectorVariableValuePairs
	| literalIndex values |
	literalIndex := 0.
	values := OrderedCollection new.
	values addAll: (self literals collect: [ :aLiteral | 'literal', ( literalIndex := literalIndex + 1 ) asString -> aLiteral ]).
	self initialPC to: self size do: [ :index |
		values add:  ('bc ', index asString) ->  (self at: index)].
	^values.
	
