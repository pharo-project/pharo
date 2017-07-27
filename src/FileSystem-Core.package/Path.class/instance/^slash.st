navigating
/ aString
	| path |

	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].
		
	(aString includes:$/ ) ifTrue: [ self error:' / shouldn''t be used in the String. The string should be the last element of a path only. Refer to Path>>with:'].
		
	path := self class new: self size + 1.
	path copyFrom: self.
	path at: path size put: aString.
	^ path