navigating
/ aString
	| path |

	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].
		
	path := self class new: self size + 1.
	path copyFrom: self.
	path at: path size put: aString.
	^ path