instance creation
from: senderString to: recipients about: subjectString asFollows: bodyString 

	^ (self fromRfc822: Character lf asString, bodyString)
			from: senderString;
			date: self dateStampNow;
			subject: subjectString;
			to: recipients;
			yourself.