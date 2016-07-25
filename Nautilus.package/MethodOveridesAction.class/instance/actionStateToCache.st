order
actionStateToCache
	"Return the state of the icon for caching purpose"
	^ result 
		ifNil: [ super actionStateToCache ]
		ifNotNil: [ {(IconicButtonStateHolder forNautilus: result value first). (IconicButtonStateHolder forNautilus: result value second)} ]