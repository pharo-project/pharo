private
createNonce
	| pt |
	pt := DateAndTime now asString, ':', Random new next asString.
	^ self class md5Hash: pt