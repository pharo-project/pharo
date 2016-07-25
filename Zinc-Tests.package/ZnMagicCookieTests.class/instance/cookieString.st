testing
cookieString
	^ 'PREF=ID=18ae67ce302b6317:TM=1285696140:LM=1285696140:S=hqroFGlRQR1j4Yhy; expires={1}; path=/; domain=.google.com' format: { ZnUtils httpDate: DateAndTime now + 60 days }