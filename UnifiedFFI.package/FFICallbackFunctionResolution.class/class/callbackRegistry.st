private
callbackRegistry 
	^ CallbackRegistry ifNil: [ CallbackRegistry := Dictionary new ]