filtering
filterScanline: filterType count: count

	self
		perform: (
			#(filterNone: filterHorizontal: filterVertical: filterAverage: filterPaeth:)
				at: filterType+1)
		with: count.

