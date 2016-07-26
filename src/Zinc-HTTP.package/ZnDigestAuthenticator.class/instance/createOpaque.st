private
createOpaque
	| pt |
	pt := Random new next asString, ':', DateAndTime now asString.
	^ self class md5Hash: pt