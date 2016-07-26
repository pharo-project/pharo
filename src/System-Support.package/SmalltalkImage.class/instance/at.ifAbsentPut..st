to clean later
at: aKey ifAbsentPut: aBlock 
	"Return the global at the given key. 
	If key is not included in the receiver store the result 
	of evaluating aBlock as new value."

	^globals at: aKey ifAbsentPut: aBlock