tests - DictionaryIndexAccessing
testAtIfPresent
	"self run: #testAtIfAbsent"
	
	| t collection association nonExistantKey |
	collection := self nonEmpty .
	association := collection associations anyOne.
	nonExistantKey := self keyNotIn . 
	
	t := false.
	self nonEmptyDict at: association key ifPresent: [:x | t := (x = association value)].
	self assert: t.
	
	self assert: (self nonEmptyDict at: association key ifPresent: [:x | 'ABCDEF']) =  'ABCDEF'.

	self assert: (self nonEmptyDict at: nonExistantKey  ifPresent: [:x | Error signal]) isNil
