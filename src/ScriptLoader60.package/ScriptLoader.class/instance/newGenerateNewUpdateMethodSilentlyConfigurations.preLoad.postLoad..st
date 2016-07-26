ci
newGenerateNewUpdateMethodSilentlyConfigurations: configurations preLoad: preload postLoad: postLoad
	"ScriptLoader new newGenerateNewUpdateMethod"
	
	| stream methodName preamble postscript |
	stream := ReadWriteStream on: (String new: 1000).
	methodName := 'update', self currentUpdateVersionNumber asString.

	stream 
		<< methodName << String cr
		<< Character tab << '"self new ' << methodName << '"' << String cr
		<< Character tab .
		
	preamble := self decodeFromPrint: (preload reset contents asString).
	preamble isEmptyOrNil 
		ifFalse: [ stream nextPutAll: preamble; ensureEndsWith: $.; crtab ].

	stream 
		<< 'self withUpdateLog: ''' << self commentForCurrentUpdate << '''.' << String cr
		<< Character tab << 'self loadTogether: self script' << self currentScriptVersionNumber asString << ' merge: false.' << String cr
		<< Character tab.
		
	configurations 
		do: [ :each | stream << 'self loadConfiguration: ''' << each key << ''' version: ' << each value printString << '.' ]
		separatedBy: [ stream cr ].
		
	postscript := self decodeFromPrint: (postLoad reset contents asString). 
	postscript isEmptyOrNil 
		ifFalse: [ stream nextPutAll: postscript; ensureEndsWith: $.; crtab ].
	
	stream << 'self flushCaches.' << String cr.
	
	^ stream contents