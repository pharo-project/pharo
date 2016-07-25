enumerating
entriesDo: aBlockClosure 

	^ self allStores do: [ :store | store entriesDo: aBlockClosure ]