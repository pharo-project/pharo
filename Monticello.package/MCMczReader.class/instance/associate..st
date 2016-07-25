converting
associate: tokens
	| result |
	result := Dictionary new.
	tokens pairsDo: [:key :value | 
					| tmp |
					tmp := value.
					value isString ifFalse: [tmp := value collect: [:ea | self associate: ea]].
					value = 'nil' ifTrue: [tmp := ''].
					result at: key put: tmp].
	^ result