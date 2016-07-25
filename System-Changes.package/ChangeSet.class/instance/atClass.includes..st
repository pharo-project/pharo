private
atClass: class includes: changeType

	^(changeRecords at: class name ifAbsent: [^false])
		includesChangeType: changeType