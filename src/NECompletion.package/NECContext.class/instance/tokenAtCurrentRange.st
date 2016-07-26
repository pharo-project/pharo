private
tokenAtCurrentRange
	| range token |
	range := ranges at: completionIndex.
	token := self sourceOf: range stopAt: position.
	^ token wordBefore: token size