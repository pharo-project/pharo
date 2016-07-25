private
close

	Timer ifNotNil: [ Timer terminate ].
	Timer := nil.
	class := method := tally := receivers := nil