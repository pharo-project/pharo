accessing
firstPrecodeComment
	| comment  | 
	self comments ifEmpty: [ ^nil ].
	comment :=  self comments first.
	^self source copyFrom: comment start to: comment stop.