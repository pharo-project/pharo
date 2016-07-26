tests - offset
testReadTimeWithOtherCharactersAfter
	"See https://pharo.fogbugz.com/f/cases/17693/"
	self
		assert:
			(DateAndTime
				readFrom:
					'2016-02-03 19:23:48 +0131 By: Esteban Lorenzano <estebanlm@gmail.com> Jenkins build #547'
						readStream) printString
		equals: '2016-02-03T19:23:48+01:31'