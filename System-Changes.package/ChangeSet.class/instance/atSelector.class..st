private
atSelector: selector class: class

	^ (changeRecords at: class name ifAbsent: [^ #none])
		atSelector: selector ifAbsent: [^ #none]