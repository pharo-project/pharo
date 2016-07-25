accessing
cookieAt: aName forUrl: urlObject
	(self cookiesForUrl: urlObject) do: [ :cookie | cookie name = aName ifTrue: [ ^ cookie ] ].
	^ nil