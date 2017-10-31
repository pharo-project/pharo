private
hasValidStream

	^super hasValidStream and: [ date = Date today ]