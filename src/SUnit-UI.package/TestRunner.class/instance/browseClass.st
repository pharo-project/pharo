accessing-classes
browseClass

	^ (classes at: classIndex ifAbsent: [ ^ self ]) browse