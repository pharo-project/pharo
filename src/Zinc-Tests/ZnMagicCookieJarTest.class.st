Class {
	#name : #ZnMagicCookieJarTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #accessing }
ZnMagicCookieJarTest >> cookieString [
	^ 'PREF=ID=18ae67ce302b6317:TM=1285696140:LM=1285696140:S=hqroFGlRQR1j4Yhy; expires={1}; path=/; domain=.google.com' format: { ZnUtils httpDate: DateAndTime now + 60 days }
]

{ #category : #accessing }
ZnMagicCookieJarTest >> cookieStringAlt [
	^'PREFS=ID=18ae67ce302b6317:TM=1285696140:LM=1285696140:S=hqroFGlRQR1j4Yhy; expires={1}; path=/; domain=.pharo-project.org' format: { ZnUtils httpDate: DateAndTime now + 60 days }
]

{ #category : #testing }
ZnMagicCookieJarTest >> testAdd [
	| jar count |
	jar := ZnCookieJar new.
	count := jar cookies size.
	jar add: (ZnCookie fromString: self cookieString for: 'www.google.com' asZnUrl).
	self assert: jar cookies size equals: count + 1
]

{ #category : #testing }
ZnMagicCookieJarTest >> testCookieAtForUrl [
	| jar c1 c2 |
	jar := ZnCookieJar new.
	c1 := ZnCookie fromString: self cookieString for: 'http://www.google.com' asZnUrl.
	c2 := ZnCookie fromString: self cookieStringAlt for: 'http://www.pharo-project.org' asZnUrl.
	jar add: c1; add: c2.
	self assert: ((jar cookieAt: 'PREF' forUrl: 'http://www.google.com' asZnUrl) notNil).
	self assert: ((jar cookieAt: 'foobar' forUrl: 'http://www.google.com' asZnUrl) isNil)
]

{ #category : #testing }
ZnMagicCookieJarTest >> testCookiesForUrl [
	| jar c1 c2 |
	jar := ZnCookieJar new.
	c1 := ZnCookie fromString: self cookieString for: 'http://www.google.com' asZnUrl.
	c2 := ZnCookie fromString: self cookieStringAlt for: 'http://www.pharo-project.org' asZnUrl.
	jar
		add: c1;
		add: c2.
	self assert: (jar cookiesForUrl: 'http://www.google.com' asZnUrl) size equals: 1
]
