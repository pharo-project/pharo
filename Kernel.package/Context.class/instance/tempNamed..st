accessing
tempNamed: aName
	"Returns the value of the temporaries, aName."

	"Implementation notes: temporary initialization in blocks simply uses pushNil to allocate and initialize each temp.  So if one inspects [|a|a:=2] and sends it self method symbolic you get:

	13 <8F 00 00 05> closureNumCopied: 0 numArgs: 0 bytes 17 to 21
	17 	<73> pushConstant: nil
	18 	<77> pushConstant: 2
	19 	<81 40> storeIntoTemp: 0
	21 	<7D> blockReturn
	22 <7C> returnTop

	And when we check self asContext pc we get 17, which is *before* the nil is pushed. Therefore we should pay attention when querying a temporary if the temporary allocation was executed."

	| index |
	index := self tempNames indexOf: aName.
	^ self namedTempAt: index