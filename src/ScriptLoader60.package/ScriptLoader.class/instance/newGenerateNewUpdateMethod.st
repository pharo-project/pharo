new script-update format
newGenerateNewUpdateMethod
	"ScriptLoader new newGenerateNewUpdateMethod"
	
	| str mthName preamble postscript |
	str := ReadWriteStream on: (String new: 1000).
	mthName := 'update', self currentUpdateVersionNumber asString.
	str nextPutAll: mthName ; cr  ; tab.
	str nextPutAll: '"self new ', mthName, '"'; crtab. 
	preamble := UIManager default 
		multiLineRequest: 'Preamble expression'
		initialAnswer: ''
		answerHeight: 100.
	preamble isEmptyOrNil ifFalse: [str nextPutAll: preamble; ensureEndsWith: $.; crtab].

	str nextPutAll: 'self withUpdateLog: ' ; nextPut: $'; nextPutAll: self commentForCurrentUpdate ; nextPut: $'; nextPut: $. .
	str crtab.
	str nextPutAll: 'self loadTogether: '.
	str nextPutAll: 'self script' , self currentScriptVersionNumber asString, ' merge: false.'.
	str crtab.
	postscript := UIManager default
					multiLineRequest: 'Postscript expression'
					initialAnswer: ''
					answerHeight: 100. 
	postscript isEmptyOrNil ifFalse: [str nextPutAll: postscript; ensureEndsWith: $.; crtab].
	
	str nextPutAll: 'self flushCaches.'; cr.
	str nextPutAll: 'self cleanRepositories.'; cr.
	
	
	^ str contents