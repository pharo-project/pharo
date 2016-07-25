private
sendCookies
	| cookies cookiesString |
	session ifNil: [ ^ self ].
	cookies := (session cookieJar cookiesForUrl: request url).
	cookies isEmpty ifTrue: [ ^ self ]. 
	cookiesString := String streamContents: [ :stream |
		cookies
			do: [ :each | each writeNameAndValueOn: stream ]
			separatedBy: [ stream nextPutAll: '; ' ] ].
	request setCookie: cookiesString