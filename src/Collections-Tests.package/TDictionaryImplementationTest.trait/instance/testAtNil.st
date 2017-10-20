tests - implementation
testAtNil
	"(self run: #testAtNil)"

	"nil is a valid key in Pharo. In VW nil is not a valid key"

	"Ansi 1.9 p, 168
    		5.7.2.5 Message: at: key put: newElement
    		Synopsis
    			Store newElement at key in the receiver. Answer newElement.
    		Definition: <abstractDictionary>
    		If lookup succeeds for key, then newElement replaces the element previously stored at key.
    		Otherwise, the newElement is stored at the new key. In either case, subsequent successful
    		lookups for key will answer newElement.  Answer newElement.

    		The result is undefined if the key is nil.

		This clearly indicates that different smalltalks where doing different assumptions."

	| dict1 |
	dict1 := self emptyDict.
	dict1 at: nil put: #none.
	self assert: (dict1 at: nil) = #none