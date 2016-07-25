accessing
keys
 
 	| keys |
 	keys := Set new.
 	self keysDo: [:key | keys add: key].
 	^ keys