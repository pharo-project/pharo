private
swapShortObjects
	"Private -- swap all the short quantities in the receiver"
	| tmp |
	1 to: self basicSize do:[:i|
		tmp := (self pvtAt: i * 2).
		self pvtAt: i * 2 put: (self pvtAt: i * 2 - 1).
		self pvtAt: i * 2 - 1 put: tmp.
	]