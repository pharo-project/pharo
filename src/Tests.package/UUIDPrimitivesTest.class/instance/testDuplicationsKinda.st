tests
testDuplicationsKinda
	| check size |

	size := 5000.
	check := Set new: size.
	size timesRepeat: 
		[ | uuid |uuid := UUID new.
		self shouldnt: [check includes: uuid].
		check add: uuid].
		