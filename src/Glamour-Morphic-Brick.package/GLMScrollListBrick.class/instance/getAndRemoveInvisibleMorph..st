lazy scrolling
getAndRemoveInvisibleMorph: aCollection
	|brick|
	brick := self retrieveLast: aCollection.
	^ brick ifNotNil: [ self removeBrickNoLayout: brick. brick morph ]