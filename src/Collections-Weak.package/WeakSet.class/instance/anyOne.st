accessing
anyOne
	"First we will try to return any real object which is not garbage collected. It will skip all slots with flag. In case when everything is garbage collected we will return nil. WeakSet is not empty in this case (isEmpty will return false). So we should not signal any error"
	| result |
	result := super anyOne.
	^result == self 
		ifTrue: [ nil ] 
		ifFalse: [ result ]