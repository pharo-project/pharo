Class {
	#name : #ZnMagicCookieTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnMagicCookieTest >> cookieString [
	^ 'PREF=ID=18ae67ce302b6317:TM=1285696140:LM=1285696140:S=hqroFGlRQR1j4Yhy; expires={1}; path=/; domain=.google.com' format: { ZnUtils httpDate: DateAndTime now + 60 days }
]

{ #category : #accessing }
ZnMagicCookieTest >> cookieStringSubpath [
	^ 'PREF=ID=18ae67ce302b6317:TM=1285696140:LM=1285696140:S=hqroFGlRQR1j4Yhy; expires={1}; path=/test/; domain=.google.com' format: { ZnUtils httpDate: DateAndTime now + 60 days }
]

{ #category : #testing }
ZnMagicCookieTest >> testFromString [
	| cookie |
	cookie := ZnCookie fromString: self cookieString for: 'www.google.com' asZnUrl.
	self assert: cookie name equals: 'PREF'.
	self assert: cookie path notNil.
	self assert: cookie domain equals: '.google.com'.
	self assert: cookie isExpired not
]

{ #category : #testing }
ZnMagicCookieTest >> testInDomain [
	| cookie |
	cookie := ZnCookie fromString: self cookieString for: 'http://www.google.com' asZnUrl.
	self assert: (cookie isInDomain: 'http://www.google.com' asZnUrl).
	self assert: (cookie isInDomain: 'http://google.com' asZnUrl).
	self assert: ((cookie isInDomain: 'http://pharo-project.org' asZnUrl) not)
	
]

{ #category : #testing }
ZnMagicCookieTest >> testInPath [
	| cookie |
	cookie := ZnCookie fromString: self cookieStringSubpath for: 'www.google.com' asZnUrl.
	self assert: (cookie isInPath: 'www.google.com/test/' asZnUrl).
	self assert: ((cookie isInPath: 'www.google.com' asZnUrl) not)
	
]
