converting
asDictionary
	^ Dictionary new
		at: #name put: name;
		at: #id put: id asString;
		at: #message put: message;
		at: #date put: date;
		at: #time put: time;
		at: #author put: author;
		at: #ancestors put: (self ancestors collect: [:a | a asDictionary]);
		yourself