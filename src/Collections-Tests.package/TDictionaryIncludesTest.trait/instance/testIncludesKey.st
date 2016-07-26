tests - dictionary including
testIncludesKey

	| collection keyIn nonExistantKey |

	collection := self nonEmpty .
	keyIn := collection keys anyOne.
	nonExistantKey := self keyNotInNonEmpty. 

	self assert: ( collection includesKey: keyIn ).
	self deny: ( collection includesKey: nonExistantKey ).