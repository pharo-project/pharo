Replaces keys/values do: by keysDo: and valuesDo:.  This rule is based on the observation that using aDict keys do: or aDict values do: create an intermediary collection. 

Dictionary>>keys
	"Answer an Array containing the receiver's keys."
	
	^Array new: self size streamContents: [:s| self keysDo: [:key| s nextPut: key]]
	
This array can be quite large and using the keysDo: does not create such intermediate collection.