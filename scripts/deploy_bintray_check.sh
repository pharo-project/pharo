#! /bin/bash

./pharo Pharo.image eval "
| json username password url currentMonth stdout newLine |

stdout := VTermOutputDriver stdout.
newLine := Smalltalk platform lineEnding.

stdout green: 'Cleaning bintray...', newLine.

username := '$BINTRAY_USER'.
password := '$BINTRAY_API_KEY'.
url := 'https://api.bintray.com/packages/pharo-project/pharo/Pharo'.
currentMonth := Month current year * 100 + Month current index.

json := ZnClient new
	username: username password: password;
	url: url;
	contentReader: [ :entity | STON fromString: entity contents ];
	get.

(json at: 'versions')
	select: [ :each | (each first: 6) asInteger < currentMonth ]
	thenDo: [ :each | | response |
		response := ZnClient new
			username: username password: password;
			url: (url, '/versions/', each);
			delete;
			response.
		response isSuccess
			ifTrue: [ stdout green: 'Version ', each, ' deleted.', newLine ]
			ifFalse: [ stdout red: 'Error while trying to remove ', each, '!', newLine ] ].

stdout green: 'Done.', newLine.
"
