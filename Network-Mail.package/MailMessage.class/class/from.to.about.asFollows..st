instance creation
from: senderString to: recipients about: subjectString asFollows: bodyString 

	| recipientsString |
	recipientsString := String streamContents: [ :str |
		recipients do: [ :e | str nextPutAll: e ] separatedBy: [ str nextPutAll: ', ' ] ].
	^ (self from: Character lf asString, bodyString)
			setField: 'from' toString: senderString;
			setField: 'date' toString: self dateStampNow;
			setField: 'subject' toString: subjectString;
			setField: 'to' toString: recipientsString;
			yourself.