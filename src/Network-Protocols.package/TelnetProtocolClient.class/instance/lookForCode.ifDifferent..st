private protocol
lookForCode: code ifDifferent: handleBlock
	"We are expecting a certain code next."

	self fetchNextResponse.

	self responseCode == code
		ifFalse: [handleBlock value: self lastResponse]
