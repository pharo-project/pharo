instructions
returnReceiver
	bytes ifEmpty: [ lastSpecialReturn := Message selector: #returnReceiver].
	encoder genReturnReceiver.
